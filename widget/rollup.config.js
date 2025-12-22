import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import replace from '@rollup/plugin-replace';
import { babel } from '@rollup/plugin-babel';
import terser from '@rollup/plugin-terser';

export default {
  input: 'src/dotVisualization.jsx',
  output: {
    file: '../build/js/dotVisualization.js',
    format: 'es',
    sourcemap: false,
  },
  external: ['react', 'react-dom', 'react/jsx-runtime', '@leanprover/infoview'],
  plugins: [
    replace({
      'process.env.NODE_ENV': JSON.stringify('production'),
      preventAssignment: true,
    }),
    resolve({
      browser: true,
      preferBuiltins: false,
    }),
    commonjs(),
    babel({
      babelHelpers: 'bundled',
      presets: ['@babel/preset-react'],
      extensions: ['.js', '.jsx'],
    }),
    terser(),
  ],
};
