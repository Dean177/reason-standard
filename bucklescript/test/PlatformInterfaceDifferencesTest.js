const path = require("path");
const { exec } = require("child_process");

// There are some differences between the native and bucklescript rei files, for 
// example:
// - Bucklescript has its own Result type since the ocaml version it has 
//   forked(4.06) does not have its own.
// - Some Bucklescript values are 'external', whilst in natve they are regular 
//   values.
// - The Map & Set implementations are not provided by the standard library and 
//   their types are intentionally exposed
// 
// This test acts as a sanity check to ensure the 'rei' files stay in sync
// despite these differences.
test(`rei files don't diverge`, (done) => {
  let nativeMli = path.resolve(__dirname, '../../native/src/Standard.rei')
  let bucklescriptMli = path.resolve(__dirname, '../src/Standard.rei')
    exec(`diff ${nativeMli} ${bucklescriptMli}`, (error, stdout, stderr) => {
      if (error) {
        throw error
      }
      expect(stderr.toString()).toMatchSnapshot()
      expect(stdout.toString()).toMatchSnapshot()
      done()
  })
})
