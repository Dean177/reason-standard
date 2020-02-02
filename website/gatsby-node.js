const path = require("path");

exports.createPages = ({ graphql, actions }) => {
  const { createPage } = actions;

  createPage({
    path: '/api',
    component: path.resolve('./src/templates/api.js'),
    context: {},
  });

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
                    slug
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
            path: node.fields.slug ? node.fields.slug : "/",
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
    let value = parent.relativePath.replace(parent.ext, "");

    if (value === "index") {
      value = "";
    }

    createNodeField({
      name: `slug`,
      node,
      value: `/${value}`
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
  }
};
