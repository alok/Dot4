import React, { useEffect, useRef, useState, useCallback, useMemo, useContext } from 'react';
import { instance } from '@viz-js/viz';
import { EditorContext } from '@leanprover/infoview';

// Layout engines available in Graphviz
const ENGINES = ['dot', 'neato', 'fdp', 'sfdp', 'circo', 'twopi', 'osage', 'patchwork'];

export default function DotVisualization(props) {
  const containerRef = useRef(null);
  const svgRef = useRef(null);
  const minimapRef = useRef(null);
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(true);
  const [selectedNode, setSelectedNode] = useState(null);
  const [engine, setEngine] = useState(props.engine || 'dot');
  const [animating, setAnimating] = useState(false);
  const [highlightedNodes, setHighlightedNodes] = useState(new Set());
  const [hoveredNode, setHoveredNode] = useState(null);
  const [showMinimap, setShowMinimap] = useState(true);
  const animationRef = useRef(null);

  const dotSource = props.dotSource || 'digraph { a -> b }';
  const isDiff = props.isDiff || false;
  const addedNodes = new Set(props.addedNodes || []);
  const removedNodes = new Set(props.removedNodes || []);
  const addedEdges = new Set(props.addedEdges || []);
  const removedEdges = new Set(props.removedEdges || []);

  // Get editor connection for go-to-definition
  const editorConnection = useContext(EditorContext);

  // Build map from element ID to source location
  const sourceLocationMap = useMemo(() => {
    const map = new Map();
    if (props.sourceLocations) {
      for (const loc of props.sourceLocations) {
        map.set(loc.id, {
          uri: loc.uri,
          range: {
            start: { line: loc.startLine, character: loc.startChar },
            end: { line: loc.endLine, character: loc.endChar }
          }
        });
      }
    }
    return map;
  }, [props.sourceLocations]);

  // Navigate to source location on click
  const goToSource = useCallback((elementId) => {
    const loc = sourceLocationMap.get(elementId);
    if (loc && editorConnection) {
      editorConnection.revealLocation(loc);
    }
  }, [sourceLocationMap, editorConnection]);

  // Detect dark mode from VS Code theme
  const isDarkMode = () => {
    return document.body.classList.contains('vscode-dark') ||
           document.body.getAttribute('data-vscode-theme-kind') === 'vscode-dark';
  };

  const [darkMode, setDarkMode] = useState(isDarkMode());

  // Listen for theme changes
  useEffect(() => {
    const observer = new MutationObserver(() => {
      setDarkMode(isDarkMode());
    });
    observer.observe(document.body, { attributes: true, attributeFilter: ['class', 'data-vscode-theme-kind'] });
    return () => observer.disconnect();
  }, []);

  // Parse graph structure for neighbor highlighting
  const graphStructure = useMemo(() => {
    const predecessors = new Map(); // node -> Set of nodes that point TO it
    const successors = new Map();   // node -> Set of nodes it points TO
    const edgeMap = new Map();      // "src->dst" -> true

    // Parse edges from DOT source
    // Matches: "a" -> "b", a -> b, "node1" -> "node2", etc.
    const edgeRegex = /["']?([^"'\s\[\]{}]+)["']?\s*-[->]\s*["']?([^"'\s\[\]{}]+)["']?/g;
    let match;
    while ((match = edgeRegex.exec(dotSource)) !== null) {
      const [, src, dst] = match;
      if (src && dst && src !== '{' && dst !== '}') {
        if (!successors.has(src)) successors.set(src, new Set());
        if (!predecessors.has(dst)) predecessors.set(dst, new Set());
        successors.get(src).add(dst);
        predecessors.get(dst).add(src);
        edgeMap.set(`${src}->${dst}`, true);
        edgeMap.set(`${src}--${dst}`, true); // undirected variant
      }
    }

    return { predecessors, successors, edgeMap };
  }, [dotSource]);

  // Get neighbors of a node
  const getNeighbors = useCallback((nodeId) => {
    const preds = graphStructure.predecessors.get(nodeId) || new Set();
    const succs = graphStructure.successors.get(nodeId) || new Set();
    return { predecessors: preds, successors: succs };
  }, [graphStructure]);

  // Handle node/edge clicks for interactivity
  const setupInteractivity = useCallback((svgElement) => {
    // Make nodes clickable
    svgElement.querySelectorAll('g.node').forEach(node => {
      const title = node.querySelector('title');
      const nodeId = title ? title.textContent : 'unknown';
      const hasSourceLoc = sourceLocationMap.has(nodeId);

      node.style.cursor = hasSourceLoc ? 'pointer' : 'default';

      node.addEventListener('click', (e) => {
        e.stopPropagation();
        const label = node.querySelector('text')?.textContent || nodeId;
        const shape = node.querySelector('ellipse') ? 'ellipse' :
                      node.querySelector('polygon') ? 'polygon' :
                      node.querySelector('path') ? 'path' : 'unknown';
        setSelectedNode({
          type: 'node',
          id: nodeId,
          label,
          shape,
          hasSourceLoc
        });
      });

      // Double-click to go to source
      node.addEventListener('dblclick', (e) => {
        e.stopPropagation();
        goToSource(nodeId);
      });

      // Hover effect with neighbor highlighting
      node.addEventListener('mouseenter', () => {
        node.style.opacity = '0.8';
        setHoveredNode(nodeId);
      });
      node.addEventListener('mouseleave', () => {
        node.style.opacity = '1';
        setHoveredNode(null);
      });
    });

    // Make edges clickable
    svgElement.querySelectorAll('g.edge').forEach(edge => {
      const title = edge.querySelector('title');
      const edgeId = title ? title.textContent : 'unknown';
      const hasSourceLoc = sourceLocationMap.has(edgeId);

      edge.style.cursor = hasSourceLoc ? 'pointer' : 'default';

      edge.addEventListener('click', (e) => {
        e.stopPropagation();
        const [src, dst] = edgeId.split('->').map(s => s.trim());
        const label = edge.querySelector('text')?.textContent || '';
        setSelectedNode({
          type: 'edge',
          id: edgeId,
          src,
          dst,
          label,
          hasSourceLoc
        });
      });

      // Double-click to go to source
      edge.addEventListener('dblclick', (e) => {
        e.stopPropagation();
        goToSource(edgeId);
      });
    });

    // Click elsewhere to deselect
    svgElement.addEventListener('click', () => {
      setSelectedNode(null);
    });
  }, [sourceLocationMap, goToSource]);

  // Apply diff highlighting
  const applyDiffHighlighting = useCallback((svgElement) => {
    if (!isDiff) return;

    svgElement.querySelectorAll('g.node').forEach(node => {
      const title = node.querySelector('title');
      const nodeId = title ? title.textContent : '';

      if (addedNodes.has(nodeId)) {
        // Green for added
        node.querySelectorAll('ellipse, polygon, path').forEach(el => {
          el.style.stroke = '#4caf50';
          el.style.strokeWidth = '3';
          el.style.fill = '#e8f5e9';
        });
      } else if (removedNodes.has(nodeId)) {
        // Red for removed
        node.querySelectorAll('ellipse, polygon, path').forEach(el => {
          el.style.stroke = '#f44336';
          el.style.strokeWidth = '3';
          el.style.fill = '#ffebee';
          el.style.strokeDasharray = '5,5';
        });
      }
    });

    svgElement.querySelectorAll('g.edge').forEach(edge => {
      const title = edge.querySelector('title');
      const edgeId = title ? title.textContent : '';

      if (addedEdges.has(edgeId)) {
        edge.querySelectorAll('path, polygon').forEach(el => {
          el.style.stroke = '#4caf50';
          el.style.strokeWidth = '2';
        });
      } else if (removedEdges.has(edgeId)) {
        edge.querySelectorAll('path, polygon').forEach(el => {
          el.style.stroke = '#f44336';
          el.style.strokeWidth = '2';
          el.style.strokeDasharray = '5,5';
        });
      }
    });
  }, [isDiff, addedNodes, removedNodes, addedEdges, removedEdges]);

  // Export SVG
  const exportSVG = useCallback(() => {
    const svg = svgRef.current;
    if (!svg) return;

    const serializer = new XMLSerializer();
    const svgString = serializer.serializeToString(svg);
    const blob = new Blob([svgString], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = 'graph.svg';
    a.click();
    URL.revokeObjectURL(url);
  }, []);

  // Export PNG
  const exportPNG = useCallback(() => {
    const svg = svgRef.current;
    if (!svg) return;

    const serializer = new XMLSerializer();
    const svgString = serializer.serializeToString(svg);
    const svgBlob = new Blob([svgString], { type: 'image/svg+xml;charset=utf-8' });
    const url = URL.createObjectURL(svgBlob);

    const img = new Image();
    img.onload = () => {
      const canvas = document.createElement('canvas');
      const scale = 2; // Higher resolution
      canvas.width = img.width * scale;
      canvas.height = img.height * scale;

      const ctx = canvas.getContext('2d');
      ctx.scale(scale, scale);
      ctx.fillStyle = darkMode ? '#1e1e1e' : 'white';
      ctx.fillRect(0, 0, img.width, img.height);
      ctx.drawImage(img, 0, 0);

      canvas.toBlob((blob) => {
        const pngUrl = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = pngUrl;
        a.download = 'graph.png';
        a.click();
        URL.revokeObjectURL(pngUrl);
      }, 'image/png');

      URL.revokeObjectURL(url);
    };
    img.src = url;
  }, [darkMode]);

  // Animation: highlight nodes in sequence (for topological sort visualization)
  const animateNodes = useCallback((nodeOrder) => {
    if (animating || !nodeOrder || nodeOrder.length === 0) return;

    setAnimating(true);
    let index = 0;

    const step = () => {
      if (index >= nodeOrder.length) {
        setAnimating(false);
        setHighlightedNodes(new Set());
        return;
      }

      setHighlightedNodes(new Set(nodeOrder.slice(0, index + 1)));
      index++;
      animationRef.current = setTimeout(step, 500);
    };

    step();
  }, [animating]);

  // Stop animation
  const stopAnimation = useCallback(() => {
    if (animationRef.current) {
      clearTimeout(animationRef.current);
    }
    setAnimating(false);
    setHighlightedNodes(new Set());
  }, []);

  // Apply animation highlighting to SVG
  const applyAnimationHighlighting = useCallback((svgElement) => {
    if (highlightedNodes.size === 0) return;

    svgElement.querySelectorAll('g.node').forEach(node => {
      const title = node.querySelector('title');
      const nodeId = title ? title.textContent : '';

      if (highlightedNodes.has(nodeId)) {
        node.querySelectorAll('ellipse, polygon, path').forEach(el => {
          el.style.stroke = '#ff9800';
          el.style.strokeWidth = '3';
          el.style.fill = darkMode ? '#4a3000' : '#fff3e0';
        });
        node.querySelectorAll('text').forEach(t => {
          t.style.fill = '#ff9800';
          t.style.fontWeight = 'bold';
        });
      }
    });
  }, [highlightedNodes, darkMode]);

  // Apply neighbor highlighting on hover
  const applyNeighborHighlighting = useCallback((svgElement) => {
    if (!hoveredNode) return;

    const { predecessors, successors } = getNeighbors(hoveredNode);

    // Highlight predecessor nodes (blue - incoming)
    svgElement.querySelectorAll('g.node').forEach(node => {
      const title = node.querySelector('title');
      const nodeId = title ? title.textContent : '';

      if (predecessors.has(nodeId)) {
        node.querySelectorAll('ellipse, polygon, path').forEach(el => {
          el.style.stroke = '#2196f3';
          el.style.strokeWidth = '3';
          el.style.filter = 'drop-shadow(0 0 4px #2196f3)';
        });
        node.querySelectorAll('text').forEach(t => {
          t.style.fill = '#2196f3';
          t.style.fontWeight = 'bold';
        });
      } else if (successors.has(nodeId)) {
        // Highlight successor nodes (orange - outgoing)
        node.querySelectorAll('ellipse, polygon, path').forEach(el => {
          el.style.stroke = '#ff9800';
          el.style.strokeWidth = '3';
          el.style.filter = 'drop-shadow(0 0 4px #ff9800)';
        });
        node.querySelectorAll('text').forEach(t => {
          t.style.fill = '#ff9800';
          t.style.fontWeight = 'bold';
        });
      } else if (nodeId !== hoveredNode) {
        // Dim non-related nodes
        node.style.opacity = '0.3';
      }
    });

    // Highlight edges connected to hovered node
    svgElement.querySelectorAll('g.edge').forEach(edge => {
      const title = edge.querySelector('title');
      const edgeId = title ? title.textContent : '';
      const [src, dst] = edgeId.split('->').map(s => s.trim());

      if (src === hoveredNode) {
        // Outgoing edge (orange)
        edge.querySelectorAll('path, polygon').forEach(el => {
          el.style.stroke = '#ff9800';
          el.style.strokeWidth = '2.5';
          el.style.filter = 'drop-shadow(0 0 3px #ff9800)';
        });
      } else if (dst === hoveredNode) {
        // Incoming edge (blue)
        edge.querySelectorAll('path, polygon').forEach(el => {
          el.style.stroke = '#2196f3';
          el.style.strokeWidth = '2.5';
          el.style.filter = 'drop-shadow(0 0 3px #2196f3)';
        });
      } else {
        // Dim non-related edges
        edge.style.opacity = '0.2';
      }
    });
  }, [hoveredNode, getNeighbors]);

  // Render minimap
  const renderMinimap = useCallback(() => {
    if (!svgRef.current || !minimapRef.current || !showMinimap) return;

    const svg = svgRef.current;
    const canvas = minimapRef.current;
    const ctx = canvas.getContext('2d');

    // Get SVG dimensions
    const svgRect = svg.getBoundingClientRect();
    const viewBox = svg.getAttribute('viewBox');
    let svgWidth, svgHeight;

    if (viewBox) {
      const [, , w, h] = viewBox.split(' ').map(Number);
      svgWidth = w;
      svgHeight = h;
    } else {
      svgWidth = svgRect.width;
      svgHeight = svgRect.height;
    }

    // Set canvas size (minimap is 150px wide max)
    const maxWidth = 150;
    const scale = Math.min(maxWidth / svgWidth, 100 / svgHeight);
    canvas.width = svgWidth * scale;
    canvas.height = svgHeight * scale;

    // Clear canvas
    ctx.fillStyle = darkMode ? '#1e1e1e' : '#f5f5f5';
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Draw simplified nodes
    svg.querySelectorAll('g.node').forEach(node => {
      const ellipse = node.querySelector('ellipse');
      const polygon = node.querySelector('polygon');
      const title = node.querySelector('title');
      const nodeId = title ? title.textContent : '';

      let cx, cy, rx, ry;

      if (ellipse) {
        cx = parseFloat(ellipse.getAttribute('cx')) * scale;
        cy = parseFloat(ellipse.getAttribute('cy')) * scale;
        rx = Math.max(parseFloat(ellipse.getAttribute('rx')) * scale, 2);
        ry = Math.max(parseFloat(ellipse.getAttribute('ry')) * scale, 2);
      } else if (polygon) {
        const points = polygon.getAttribute('points').split(' ').map(p => {
          const [x, y] = p.split(',').map(Number);
          return { x: x * scale, y: y * scale };
        });
        cx = points.reduce((sum, p) => sum + p.x, 0) / points.length;
        cy = points.reduce((sum, p) => sum + p.y, 0) / points.length;
        rx = ry = 3;
      } else {
        return;
      }

      // Determine node color based on state
      if (nodeId === hoveredNode) {
        ctx.fillStyle = '#4caf50';
      } else if (hoveredNode) {
        const { predecessors, successors } = getNeighbors(hoveredNode);
        if (predecessors.has(nodeId)) {
          ctx.fillStyle = '#2196f3';
        } else if (successors.has(nodeId)) {
          ctx.fillStyle = '#ff9800';
        } else {
          ctx.fillStyle = darkMode ? '#555' : '#999';
        }
      } else {
        ctx.fillStyle = darkMode ? '#888' : '#666';
      }

      ctx.beginPath();
      ctx.ellipse(cx, cy, rx, ry, 0, 0, 2 * Math.PI);
      ctx.fill();
    });

    // Draw simplified edges
    ctx.strokeStyle = darkMode ? '#444' : '#ccc';
    ctx.lineWidth = 0.5;
    svg.querySelectorAll('g.edge path').forEach(path => {
      const d = path.getAttribute('d');
      if (!d) return;

      // Simple path parsing - just get the start and end points
      const moves = d.match(/[MC]\s*[\d.,\s-]+/g);
      if (!moves || moves.length === 0) return;

      ctx.beginPath();
      moves.forEach((cmd, i) => {
        const coords = cmd.substring(1).trim().split(/[\s,]+/).map(Number);
        if (coords.length >= 2) {
          const x = coords[coords.length - 2] * scale;
          const y = coords[coords.length - 1] * scale;
          if (i === 0) {
            ctx.moveTo(x, y);
          } else {
            ctx.lineTo(x, y);
          }
        }
      });
      ctx.stroke();
    });

    // Draw border
    ctx.strokeStyle = darkMode ? '#555' : '#ddd';
    ctx.lineWidth = 1;
    ctx.strokeRect(0, 0, canvas.width, canvas.height);
  }, [showMinimap, darkMode, hoveredNode, getNeighbors]);

  // Update minimap when SVG changes
  useEffect(() => {
    if (!loading) {
      renderMinimap();
    }
  }, [loading, renderMinimap, hoveredNode]);

  // Get node order from DOT (parse node declarations)
  const getNodeOrder = useCallback(() => {
    const nodeMatches = dotSource.matchAll(/"([^"]+)"\s*\[/g);
    const nodes = [];
    for (const match of nodeMatches) {
      if (!nodes.includes(match[1])) {
        nodes.push(match[1]);
      }
    }
    return nodes;
  }, [dotSource]);

  useEffect(() => {
    let mounted = true;

    async function renderGraph() {
      try {
        setLoading(true);
        setError(null);

        const viz = await instance();
        const svgElement = viz.renderSVGElement(dotSource, { engine });

        if (mounted && containerRef.current) {
          containerRef.current.innerHTML = '';

          // Apply theme-aware styling
          svgElement.style.maxWidth = '100%';
          svgElement.style.height = 'auto';
          svgElement.style.borderRadius = '4px';
          svgElement.style.padding = '8px';

          if (darkMode) {
            svgElement.style.backgroundColor = '#1e1e1e';
            // Invert colors for dark mode
            svgElement.querySelectorAll('text').forEach(t => {
              t.style.fill = '#d4d4d4';
            });
            svgElement.querySelectorAll('ellipse, polygon, path').forEach(el => {
              if (el.getAttribute('stroke') === 'black' || !el.getAttribute('stroke')) {
                el.style.stroke = '#d4d4d4';
              }
              if (el.getAttribute('fill') === 'none' || el.getAttribute('fill') === 'white') {
                el.style.fill = 'transparent';
              }
            });
          } else {
            svgElement.style.backgroundColor = 'white';
          }

          setupInteractivity(svgElement);
          applyDiffHighlighting(svgElement);
          applyAnimationHighlighting(svgElement);
          applyNeighborHighlighting(svgElement);

          svgRef.current = svgElement;
          containerRef.current.appendChild(svgElement);
          setLoading(false);
        }
      } catch (err) {
        if (mounted) {
          setError(err.message || 'Failed to render graph');
          setLoading(false);
        }
      }
    }

    renderGraph();
    return () => { mounted = false; };
  }, [dotSource, engine, darkMode, hoveredNode, setupInteractivity, applyDiffHighlighting, applyAnimationHighlighting, applyNeighborHighlighting]);

  // Error display with better formatting
  if (error) {
    // Parse error for line/column info
    const lineMatch = error.match(/line (\d+)/i);
    const colMatch = error.match(/col(?:umn)? (\d+)/i);

    return (
      <div style={{
        padding: '12px',
        backgroundColor: darkMode ? '#3d1f1f' : '#ffebee',
        color: darkMode ? '#ff8a80' : '#c62828',
        borderRadius: '4px',
        fontFamily: 'monospace',
        fontSize: '12px'
      }}>
        <strong>Graphviz Error:</strong>
        {lineMatch && <span style={{ marginLeft: '8px', opacity: 0.8 }}>Line {lineMatch[1]}</span>}
        {colMatch && <span style={{ marginLeft: '4px', opacity: 0.8 }}>Col {colMatch[1]}</span>}
        <pre style={{ margin: '8px 0 0 0', whiteSpace: 'pre-wrap' }}>{error}</pre>
      </div>
    );
  }

  return (
    <div style={{ minHeight: '100px' }}>
      {/* Engine selector toolbar */}
      <div style={{
        display: 'flex',
        gap: '4px',
        marginBottom: '8px',
        flexWrap: 'wrap',
        alignItems: 'center'
      }}>
        <span style={{
          fontSize: '11px',
          opacity: 0.7,
          color: darkMode ? '#d4d4d4' : '#333'
        }}>Layout:</span>
        {ENGINES.map(eng => (
          <button
            key={eng}
            onClick={() => setEngine(eng)}
            style={{
              padding: '2px 8px',
              fontSize: '11px',
              border: engine === eng ? '1px solid #007acc' : '1px solid transparent',
              borderRadius: '3px',
              backgroundColor: engine === eng
                ? (darkMode ? '#264f78' : '#e3f2fd')
                : (darkMode ? '#3c3c3c' : '#f5f5f5'),
              color: darkMode ? '#d4d4d4' : '#333',
              cursor: 'pointer'
            }}
          >
            {eng}
          </button>
        ))}
        {isDiff && (
          <span style={{
            marginLeft: '16px',
            fontSize: '11px',
            color: darkMode ? '#d4d4d4' : '#333'
          }}>
            <span style={{ color: '#4caf50' }}>● Added</span>
            {' '}
            <span style={{ color: '#f44336' }}>● Removed</span>
          </span>
        )}

        {/* Spacer */}
        <div style={{ flex: 1 }} />

        {/* Export & Animation buttons */}
        <button
          onClick={() => setShowMinimap(!showMinimap)}
          style={{
            padding: '2px 8px',
            fontSize: '11px',
            border: showMinimap ? '1px solid #007acc' : '1px solid transparent',
            borderRadius: '3px',
            backgroundColor: showMinimap
              ? (darkMode ? '#264f78' : '#e3f2fd')
              : (darkMode ? '#3c3c3c' : '#f5f5f5'),
            color: darkMode ? '#d4d4d4' : '#333',
            cursor: 'pointer'
          }}
          title="Toggle minimap"
        >
          🗺️
        </button>
        <button
          onClick={exportSVG}
          disabled={loading}
          style={{
            padding: '2px 8px',
            fontSize: '11px',
            border: '1px solid transparent',
            borderRadius: '3px',
            backgroundColor: darkMode ? '#3c3c3c' : '#f5f5f5',
            color: darkMode ? '#d4d4d4' : '#333',
            cursor: 'pointer'
          }}
          title="Export as SVG"
        >
          📥 SVG
        </button>
        <button
          onClick={exportPNG}
          disabled={loading}
          style={{
            padding: '2px 8px',
            fontSize: '11px',
            border: '1px solid transparent',
            borderRadius: '3px',
            backgroundColor: darkMode ? '#3c3c3c' : '#f5f5f5',
            color: darkMode ? '#d4d4d4' : '#333',
            cursor: 'pointer'
          }}
          title="Export as PNG"
        >
          📥 PNG
        </button>
        {!animating ? (
          <button
            onClick={() => animateNodes(props.animationOrder || getNodeOrder())}
            disabled={loading}
            style={{
              padding: '2px 8px',
              fontSize: '11px',
              border: '1px solid transparent',
              borderRadius: '3px',
              backgroundColor: darkMode ? '#3c3c3c' : '#f5f5f5',
              color: darkMode ? '#d4d4d4' : '#333',
              cursor: 'pointer'
            }}
            title="Animate node traversal"
          >
            ▶ Animate
          </button>
        ) : (
          <button
            onClick={stopAnimation}
            style={{
              padding: '2px 8px',
              fontSize: '11px',
              border: '1px solid #ff9800',
              borderRadius: '3px',
              backgroundColor: darkMode ? '#4a3000' : '#fff3e0',
              color: '#ff9800',
              cursor: 'pointer'
            }}
            title="Stop animation"
          >
            ⏹ Stop
          </button>
        )}
      </div>

      {loading && (
        <div style={{ padding: '12px', color: darkMode ? '#888' : '#666' }}>
          Rendering graph...
        </div>
      )}

      <div style={{ position: 'relative' }}>
        <div ref={containerRef} />

        {/* Minimap */}
        {showMinimap && !loading && (
          <div style={{
            position: 'absolute',
            top: '8px',
            right: '8px',
            borderRadius: '4px',
            overflow: 'hidden',
            boxShadow: darkMode
              ? '0 2px 8px rgba(0,0,0,0.5)'
              : '0 2px 8px rgba(0,0,0,0.15)',
            border: darkMode ? '1px solid #444' : '1px solid #ddd'
          }}>
            <canvas
              ref={minimapRef}
              style={{
                display: 'block',
                maxWidth: '150px',
                maxHeight: '100px'
              }}
            />
          </div>
        )}
      </div>

      {/* Hover info - neighbor counts */}
      {hoveredNode && (
        <div style={{
          marginTop: '8px',
          padding: '8px',
          backgroundColor: darkMode ? '#2d2d2d' : '#f5f5f5',
          borderRadius: '4px',
          fontSize: '12px',
          fontFamily: 'monospace',
          color: darkMode ? '#d4d4d4' : '#333',
          display: 'flex',
          gap: '16px',
          alignItems: 'center'
        }}>
          <span><strong>{hoveredNode}</strong></span>
          <span style={{ color: '#2196f3' }}>
            ← {getNeighbors(hoveredNode).predecessors.size} in
          </span>
          <span style={{ color: '#ff9800' }}>
            → {getNeighbors(hoveredNode).successors.size} out
          </span>
        </div>
      )}

      {/* Selected element details */}
      {selectedNode && !hoveredNode && (
        <div style={{
          marginTop: '8px',
          padding: '8px',
          backgroundColor: darkMode ? '#2d2d2d' : '#f5f5f5',
          borderRadius: '4px',
          fontSize: '12px',
          fontFamily: 'monospace',
          color: darkMode ? '#d4d4d4' : '#333'
        }}>
          {selectedNode.type === 'node' ? (
            <div style={{ display: 'flex', alignItems: 'flex-start', gap: '12px' }}>
              <div>
                <strong>Node:</strong> {selectedNode.id}<br/>
                <strong>Label:</strong> {selectedNode.label}<br/>
                <strong>Shape:</strong> {selectedNode.shape}
              </div>
              {selectedNode.hasSourceLoc && (
                <button
                  onClick={() => goToSource(selectedNode.id)}
                  style={{
                    padding: '4px 8px',
                    fontSize: '11px',
                    border: '1px solid #007acc',
                    borderRadius: '3px',
                    backgroundColor: darkMode ? '#264f78' : '#e3f2fd',
                    color: darkMode ? '#d4d4d4' : '#333',
                    cursor: 'pointer',
                    whiteSpace: 'nowrap'
                  }}
                  title="Go to source definition (or double-click node)"
                >
                  📍 Go to source
                </button>
              )}
            </div>
          ) : (
            <div style={{ display: 'flex', alignItems: 'flex-start', gap: '12px' }}>
              <div>
                <strong>Edge:</strong> {selectedNode.src} → {selectedNode.dst}<br/>
                {selectedNode.label && <><strong>Label:</strong> {selectedNode.label}</>}
              </div>
              {selectedNode.hasSourceLoc && (
                <button
                  onClick={() => goToSource(selectedNode.id)}
                  style={{
                    padding: '4px 8px',
                    fontSize: '11px',
                    border: '1px solid #007acc',
                    borderRadius: '3px',
                    backgroundColor: darkMode ? '#264f78' : '#e3f2fd',
                    color: darkMode ? '#d4d4d4' : '#333',
                    cursor: 'pointer',
                    whiteSpace: 'nowrap'
                  }}
                  title="Go to source definition (or double-click edge)"
                >
                  📍 Go to source
                </button>
              )}
            </div>
          )}
        </div>
      )}
    </div>
  );
}
