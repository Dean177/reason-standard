const chokidar = require('chokidar')

exports.sourceNodes = ({ actions }) => {
  const { createNode } = actions
  return
}

const inDevelopMode = process.env.gatsby_executing_command === 'develop'
if (inDevelopMode) {
  console.info('[odoc] Watch mode enabled, starting a listener')
  chokidar.watch('.').on('all', (event, path) => {
    console.log(event, path)
  })
}