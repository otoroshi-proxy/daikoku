// @ts-expect-error TS(2451): Cannot redeclare block-scoped variable 'Bundler'.
const Bundler = require('parcel-bundler');
// @ts-expect-error TS(2451): Cannot redeclare block-scoped variable 'Path'.
const Path = require('path');

process.env.NODE_ENV = 'production';
process.on('unhandledRejection', (reason, p) => {
  console.log('Unhandled Rejection at: Promise', p, 'reason:', reason);
});

// @ts-expect-error TS(2451): Cannot redeclare block-scoped variable 'entryFiles... Remove this comment to see the full error message
const entryFiles = Path.join(__dirname, '../src/index.js');

const options = {
  minify: true,
  cache: true,
  sourceMaps: true,
  autoinstall: true,
  contentHash: true,
  outFile: 'dist/daikoku.min.js',
  global: 'Daikoku',
  production: true,
  throwErrors: false,
  scopeHoist: false,
  target: 'browser' 
};

const bundler = new Bundler(entryFiles, options);
bundler.bundle();