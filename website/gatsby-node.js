const path = require("path");

exports.createPages = ({ graphql, actions }) => {
  const { createPage, createRedirect } = actions;

  createRedirect({
    fromPath: '/docs',
    toPath: '/docs/installation',
    redirectInBrowser: true,
  })

  return new Promise((resolve, reject) => {
    resolve(
      graphql(
        `
          {
            allMdx {
              edges {
                node {
                  fields {
                    id
                  }
                  tableOfContents
                  fields {
                    url
                  }
                }
              }
            }
          }
        `
      ).then(result => {
        if (result.errors) {
          console.log(result.errors); // eslint-disable-line no-console
          reject(result.errors);
        }

        // Create documentation pages.
        result.data.allMdx.edges.forEach(({ node }) => {
          createPage({
            path: node.fields.url,
            component: path.resolve("./src/templates/docs.js"),
            context: {
              id: node.fields.id
            }
          });
        });
      })
    );
  });
};

exports.onCreateNode = ({ node, getNode, actions }) => {
  const { createNodeField } = actions;

  if (node.internal.type === `Mdx`) {
    const parent = getNode(node.parent);
    createNodeField({
      name: `url`,
      node,
      value: `/docs/${parent.relativePath.replace(parent.ext, "")}`
    });

    createNodeField({
      name: "id",
      node,
      value: node.id
    });

    createNodeField({
      name: "title",
      node,
      value: node.frontmatter.title || (parent.name)
    });

    createNodeField({
      name: 'order',
      node,
      value: node.frontmatter.order || '999',
    });
  }
};

// refmt does a `require('fs')` but doesn't actually use the module
// Prevent this from causing an error by mocking the module
// https://www.npmjs.com/package/reason#javascript-api
exports.onCreateWebpackConfig = ({ actions }) => {
  actions.setWebpackConfig({
    node: {
      fs: 'empty',
    },
  });
};

// TODO make this into a plugin?
const chokidar = require('chokidar');
const crypto = require('crypto');
const fs = require('fs');

let log = {
  info: (...rest) => console.info('[odoc]', ...rest),
};

let id = 'odoc-model';

let createNodeFromModel = model => ({
  id,
  internal: {
    type: 'OdocModel',
    contentDigest: crypto
      .createHash(`md5`)
      .update((model))
      .digest(`hex`),
    mediaType: `application/json`,
    content: (model),
  },
});

const inDevelopMode = process.env.gatsby_executing_command === 'develop';

exports.sourceNodes = ({ actions }) => {
  let modelPath = path.resolve(__dirname, `../ocamldoc-json-generator/doc/model.json`);
  log.info('path', modelPath);
  if (modelPath == null) {
    throw new Error(`Invalid model path`);
  }
  const { createNode } = actions;
  let node = createNodeFromModel(fs.readFileSync(modelPath).toString());
  createNode(node);

  if (inDevelopMode) {
    log.info('watch mode enabled, listening for changes');
    chokidar.watch(modelPath).on('all', (event, path) => {
      log.info(event);
      if (event == 'unlink') {
        return
      }
      fs.readFile(modelPath, (error, data) => {
        if (error) {
          return log.error(error);
        }
        createNode(createNodeFromModel(data.toString()));
      });
    });
  }

  return Promise.resolve();
};