import React, { useEffect, useRef, useState } from 'react';
import { instance } from '@viz-js/viz';

export default function DotVisualization(props) {
  const containerRef = useRef(null);
  const [error, setError] = useState(null);
  const [loading, setLoading] = useState(true);

  const dotSource = props.dot || 'digraph { a -> b }';

  useEffect(() => {
    let mounted = true;

    async function renderGraph() {
      try {
        setLoading(true);
        setError(null);

        const viz = await instance();
        const svgElement = viz.renderSVGElement(dotSource);

        if (mounted && containerRef.current) {
          // Clear previous content
          containerRef.current.innerHTML = '';

          // Style the SVG for better display
          svgElement.style.maxWidth = '100%';
          svgElement.style.height = 'auto';
          svgElement.style.backgroundColor = 'white';
          svgElement.style.borderRadius = '4px';
          svgElement.style.padding = '8px';

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

    return () => {
      mounted = false;
    };
  }, [dotSource]);

  if (error) {
    return (
      <div style={{
        padding: '12px',
        backgroundColor: '#ffebee',
        color: '#c62828',
        borderRadius: '4px',
        fontFamily: 'monospace',
        fontSize: '12px'
      }}>
        <strong>Graphviz Error:</strong>
        <pre style={{ margin: '8px 0 0 0', whiteSpace: 'pre-wrap' }}>{error}</pre>
      </div>
    );
  }

  return (
    <div style={{ minHeight: '100px' }}>
      {loading && (
        <div style={{ padding: '12px', color: '#666' }}>
          Rendering graph...
        </div>
      )}
      <div ref={containerRef} />
    </div>
  );
}
