import React, { useEffect, useRef, useState, useCallback } from 'react';
import { instance } from '@viz-js/viz';

// Layout engines available in Graphviz
const ENGINES = ['dot', 'neato', 'fdp', 'sfdp', 'circo', 'twopi', 'osage', 'patchwork'];

export default function DotVisualization(props) {
  const containerRef = useRef(null);
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(true);
  const [selectedNode, setSelectedNode] = useState(null);
  const [engine, setEngine] = useState(props.engine || 'dot');

  const dotSource = props.dot || 'digraph { a -> b }';
  const isDiff = props.isDiff || false;
  const addedNodes = new Set(props.addedNodes || []);
  const removedNodes = new Set(props.removedNodes || []);
  const addedEdges = new Set(props.addedEdges || []);
  const removedEdges = new Set(props.removedEdges || []);

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

  // Handle node/edge clicks for interactivity
  const setupInteractivity = useCallback((svgElement) => {
    // Make nodes clickable
    svgElement.querySelectorAll('g.node').forEach(node => {
      node.style.cursor = 'pointer';
      const title = node.querySelector('title');
      const nodeId = title ? title.textContent : 'unknown';

      node.addEventListener('click', (e) => {
        e.stopPropagation();
        const label = node.querySelector('text')?.textContent || nodeId;
        const shape = node.querySelector('ellipse') ? 'ellipse' :
                      node.querySelector('polygon') ? 'polygon' :
                      node.querySelector('path') ? 'path' : 'unknown';
        setSelectedNode({ type: 'node', id: nodeId, label, shape });
      });

      // Hover effect
      node.addEventListener('mouseenter', () => {
        node.style.opacity = '0.8';
      });
      node.addEventListener('mouseleave', () => {
        node.style.opacity = '1';
      });
    });

    // Make edges clickable
    svgElement.querySelectorAll('g.edge').forEach(edge => {
      edge.style.cursor = 'pointer';
      const title = edge.querySelector('title');
      const edgeId = title ? title.textContent : 'unknown';

      edge.addEventListener('click', (e) => {
        e.stopPropagation();
        const [src, dst] = edgeId.split('->').map(s => s.trim());
        const label = edge.querySelector('text')?.textContent || '';
        setSelectedNode({ type: 'edge', id: edgeId, src, dst, label });
      });
    });

    // Click elsewhere to deselect
    svgElement.addEventListener('click', () => {
      setSelectedNode(null);
    });
  }, []);

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
  }, [dotSource, engine, darkMode, setupInteractivity, applyDiffHighlighting]);

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
      </div>

      {loading && (
        <div style={{ padding: '12px', color: darkMode ? '#888' : '#666' }}>
          Rendering graph...
        </div>
      )}

      <div ref={containerRef} />

      {/* Selected element details */}
      {selectedNode && (
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
            <>
              <strong>Node:</strong> {selectedNode.id}<br/>
              <strong>Label:</strong> {selectedNode.label}<br/>
              <strong>Shape:</strong> {selectedNode.shape}
            </>
          ) : (
            <>
              <strong>Edge:</strong> {selectedNode.src} → {selectedNode.dst}<br/>
              {selectedNode.label && <><strong>Label:</strong> {selectedNode.label}</>}
            </>
          )}
        </div>
      )}
    </div>
  );
}
