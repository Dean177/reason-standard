import React from "react";
import Helmet from "react-helmet";
import { graphql } from "gatsby";
import MDXRenderer from "gatsby-plugin-mdx/mdx-renderer";
import { MDXProvider } from '@mdx-js/react';
import { css} from 'styled-components'
import { LeftSidebar } from "../components/sidebar";
import { RightSidebar } from "../components/RightSideBar";
import { NextPrevious } from '../components/NextPrevious';
import mdxComponents from '../components/mdx';
import { GithubEditButton } from '../components/GithubEditButton';
import { breakpoints, colors, GlobalStyles, ThemeProvider } from '../theme';
import { NavBar, ContentContainer, AppContainer, MenuButton, PageTitle } from '../components/Layout'; 

export const pageQuery = graphql`
  query($id: String!) {
    site {
      siteMetadata {
        title
        docsLocation
        githubUrl
      }
    }
    mdx(fields: { id: { eq: $id } }) {
      fields {
        id
        title
        slug
      }
      body
      tableOfContents
      parent {
        ... on File {
          relativePath
        }
      }
      frontmatter {
        metaTitle
        metaDescription
      }
    }
    allMdx {
      edges {
        node {
          fields {
            slug
            title
          }
        }
      }
    }
  }
`;

export default ({ data }) => {
  let [isOpen, setIsOpen] = React.useState(false);
  const {
    allMdx,
    mdx,
    site: {
      siteMetadata: { docsLocation, githubUrl },
    },
  } = data;

  const navItems = allMdx.edges
    .map(({ node }) => node.fields.slug)
    .filter(slug => slug !== "/")
    .sort();

  const nav = navItems.map(slug => {
    const { node } = allMdx.edges.find(({ node }) => node.fields.slug === slug);
    return { title: node.fields.title, url: node.fields.slug };
  });

  const title = mdx.frontmatter.metaTitle;
  const description = mdx.frontmatter.metaDescription;
  // TODO static query for site url      
  // let canonicalUrl = `${config.gatsby.pathPrefix}${config.gatsby.siteUrl}${mdx.fields.slug}`;

  return (
    <ThemeProvider>
      <GlobalStyles />
      <AppContainer>
        <Helmet>
          {title ? (
            <>
              <title>{title}</title>
              <meta name="title" content={title} />
              <meta property="og:title" content={title} />
              <meta property="twitter:title" content={title} />
            </>
          ) : null}
          {description ? (
            <>
              <meta name="description" content={description} />
              <meta property="og:description" content={description} />
              <meta property="twitter:description" content={description} />
            </>
          ) : null}
          {/* TODO Static query */}
          {/* <link
            rel="shortcut icon"
            type="image/svg"
            href={config.siteMetadata.favicon}
          />  */}
          {/* <link rel="canonical" href={canonicalUrl} /> */}
        </Helmet>
        <ContentContainer>
          <NavBar githubUrl={githubUrl} />
          <div
            css={css`
              bottom: 0;
              left: 0;
              right: 0;
              top: 0;
              display: flex;
              flex: 1;
              flex-direction: column;
              position: absolute;
              z-index: ${isOpen ? 2 : -1};
              opacity: ${isOpen ? 1 : 0};
              transform: ${isOpen ? 'translateY(0)' : 'translateY(60px)'};
              /* TODO linear is weird */
              transition: all 0.2s linear;

              @media (min-width: ${breakpoints.desktop}px) {
                display: none;
              }
            `}
          >
            <LeftSidebar location={location} />
          </div>
          <main
            css={css`
              align-self: center;
              display: flex;
              flex: 1;
              flex-direction: column;
              max-width: 970px;
              padding: 0px 22px;
              padding-top: 3rem;
            `}
          >
            <PageTitle>{mdx.fields.title}</PageTitle>
            <GithubEditButton
              link={`${docsLocation}/${mdx.parent.relativePath}`}
            />
            <div
              className={css`
                background-color: pink;
                display: flex;
                flex: 1;

                ul,
                ol {
                  margin: 24px 0px;
                  padding: 0px 0px 0px 2em;
                  li {
                    font-size: 16px;
                    line-height: 1.8;
                    font-weight: 400;
                  }
                }
              `}
            >
              <MDXProvider components={mdxComponents}>
                <MDXRenderer>{mdx.body}</MDXRenderer>
              </MDXProvider>
            </div>
            <div
              css={css`
                padding: 50px 0;
              `}
            >
              <NextPrevious mdx={mdx} nav={nav} />
            </div>
          </main>
          {/* <RightSidebar location={location} /> */}
        </ContentContainer>

        <div
          css={css`
            position: absolute;
            bottom: 24px;
            right: 24px;
            z-index: 3;

            @media (min-width: ${breakpoints.desktop}px) {
              display: none;
            }
          `}
        >
          <MenuButton
            onClick={() => setIsOpen(open => !open)}
            isOpen={isOpen}
          />
        </div>
      </AppContainer>
    </ThemeProvider>
  );
}