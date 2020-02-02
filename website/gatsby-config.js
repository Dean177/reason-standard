module.exports = {
  pathPrefix: "/",
  siteMetadata: {
    title: "Standard",
    description: "A standard library for Buckelscript and native",
    ogImage: null,
    docsLocation:
      "https://github.com/Dean177/reason-standard/tree/master/content",
    favicon: "https://graphql-engine-cdn.hasura.io/img/hasura_icon_black.svg",
    headerTitle: "Standard",
    githubUrl: "https://github.com/Dean177/reason-standard",
    headerLinks: [
      { text: "Documentation", link: "/" },
      { text: "API", link: "/" },
      { text: "Github", link: "/" },
    ],
    siteUrl: "https://reason-standard.github.io",
  },
  plugins: [
    'gatsby-plugin-styled-components',
    "gatsby-plugin-sitemap",
    "gatsby-plugin-sharp",
    "gatsby-plugin-react-helmet",
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "docs",
        path: `${__dirname}/content/`,
      },
    },
    {
      resolve: "gatsby-plugin-mdx",
      options: {
        gatsbyRemarkPlugins: [
          {
            resolve: "gatsby-remark-images",
            options: {
              maxWidth: 1035,
              sizeByPixelDensity: true,
            },
          },
          {
            resolve: "gatsby-remark-copy-linked-files",
          },
        ],
        extensions: [".mdx", ".md"],
      },
    },
    {
      resolve: `gatsby-plugin-gtag`,
      options: {
        // your google analytics tracking id
        trackingId: null,
        // enable ip anonymization
        anonymize: false,
      },
    },
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: "Reason Standard",
        short_name: "Standard",
        start_url: "/",
        background_color: "#FFFFFF",
        theme_color: "#6b37bf",
        display: "standalone",
        crossOrigin: "use-credentials",
        icons: [
          {
            src: "src/pwa-512.png",
            sizes: `512x512`,
            type: `image/png`,
          },
        ],
      },
    },
    'gatsby-plugin-offline',
  ],
};
