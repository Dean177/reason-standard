import React from 'react';
import Helmet from 'react-helmet';
import { graphql } from 'gatsby';
import MDXRenderer from 'gatsby-plugin-mdx/mdx-renderer';
import { MDXProvider } from '@mdx-js/react';
import { css } from 'styled-components';
import { NextPrevious } from '../components/NextPrevious';
import { GithubEditButton } from '../components/GithubEditButton';
import { breakpoints, colors, GlobalStyles, ThemeProvider } from '../theme';
import {
  NavBar,
  ContentContainer,
  AppContainer,
  MenuButton,
  PageTitle,
} from '../components/Layout';
import {CodeBlock} from '../components/CodeBlock';
import { formatTitleToId } from '../id';
import { LeftSidebar } from '../components/sidebar';

let mdxComponents = {
  h1: props => (
    <h1 className="heading1" id={formatTitleToId(props.children)} {...props} />
  ),
  h2: props => (
    <h2 className="heading2" id={formatTitleToId(props.children)} {...props} />
  ),
  h3: props => (
    <h3 className="heading3" id={formatTitleToId(props.children)} {...props} />
  ),
  h4: props => (
    <h4 className="heading4" id={formatTitleToId(props.children)} {...props} />
  ),
  h5: props => (
    <h5 className="heading5" id={formatTitleToId(props.children)} {...props} />
  ),
  h6: props => (
    <h6 className="heading6" id={formatTitleToId(props.children)} {...props} />
  ),
  p: props => <p className="paragraph" {...props} />,
  pre: props => <pre className="pre" {...props} />,
  code: CodeBlock,
  a: ({ children: link, ...props }) => {
    return (
      <a href={props.href} target="_blank" rel="noopener" {...props}>
        {link}
      </a>
    );
  },
  img: props => <img className="img" {...props} />,
  blockquote: props => <blockquote className="blockquote" {...props} />,
};

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
        url
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
        order
      }
    }
    allMdx {
      edges {
        node {
          fields {
            url
            title
          }
          frontmatter {
            order
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

  const nav = allMdx.edges
    .filter(({ node }) => node.fields.url !== '/')
    .map(({ node }) => ({
      title: node.fields.title,
      url: node.fields.url,
      order: Number(node.frontmatter.order || '999'),
    }))
    .sort((fieldsA, fieldsB) => fieldsA.order - fieldsB.order);

  const { title } = mdx.fields;
  const { metaTitle, metaDescription } = mdx.frontmatter;  
  // TODO static query for site url
  // let canonicalUrl = `${config.gatsby.pathPrefix}${config.gatsby.siteUrl}${mdx.fields.url}`;

  return (
    <ThemeProvider>
      <GlobalStyles />
      <AppContainer>
        {/* <Helmet>
          {title ? (
            <>
              <title>{title}</title>
              <meta name="title" content={metaTitle} />
              <meta property="og:title" content={metaTitle} />
              <meta property="twitter:title" content={metaTitle} />
            </>
          ) : null}
          {metaDescription ? (
            <>
              <meta name="description" content={metaDescription} />
              <meta property="og:description" content={metaDescription} />
              <meta property="twitter:description" content={metaDescription} />
            </>
          ) : null}
        </Helmet> */}
       
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
            <LeftSidebar />
          </div>
          <main
            css={css`
              align-self: center;
              display: flex;
              flex: 1;
              flex-direction: column;
              padding: 0px 22px;
              padding-top: 3rem;
              width: 100%;
              max-width: 970px;
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
              <NextPrevious currentUrl={mdx.fields.url} nav={nav} />
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
};
