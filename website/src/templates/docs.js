import React from 'react';
import Helmet from 'react-helmet';
import { graphql } from 'gatsby';
import MDXRenderer from 'gatsby-plugin-mdx/mdx-renderer';
import { MDXProvider } from '@mdx-js/react';
import { css, createGlobalStyle } from 'styled-components';
import { NextPrevious } from '../components/NextPrevious';
import { GithubEditButton } from '../components/GithubEditButton';
import {
  breakpoints,
  colors,
  fonts,
  dimensions,
  GlobalStyles,
  ThemeProvider,
} from '../theme';
import {
  AppWrapper,
  ContentContainer,
  Main,
  MenuButton,
  MenuButtonContainer,
  NavBar,
  NavBarContainer,
  PageTitle,
  SidebarContainer,
  Container,
} from '../components/Layout';
import { CodeBlock } from '../components/CodeBlock';
import { formatTitleToId } from '../id';
import { SyntaxProvider, SyntaxToggle } from '../components/Syntax';
import { Sidebar } from '../components/Sidebar';

let MdxStyles = createGlobalStyle`
.heading1 {
  font-size: 28px;
  font-weight: 800;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.heading2 {
  font-size: 25px;
  font-weight: 700;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.heading3 {
  font-size: 20px;
  font-weight: 600;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.heading4 {
  font-size: 18px;
  font-weight: 500;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.heading5 {
  font-size: 16px;
  font-weight: 400;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.heading6 {
  font-size: 14px;
  font-weight: 300;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.paragraph {
  margin: 16px 0px;
  line-height: 1.625;
}

.pre {
  border: 0;
  font-size: 14px;
  margin: 0px;
  overflow: auto;
}

.code {
  border-radius: 4px;
  font-family: ${fonts.monospace};
  font-size: 0.9375em;
}

.blockquote {
  background-color: ${colors.grey.lighter};
  border-left: 3px solid ${colors.grey.base};
  padding-left: 14px;  
}

ul, ol {
  padding: 0px 0px 0px 2em;
  li {
    font-size: 16px;
    line-height: 1.8;
    font-weight: 400;
  }
}
`;

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
  pre: props => <pre className="pre">{props.children}</pre>,
  code: ({ className, children, ...props }) => (
    <div className="code">
      <CodeBlock
        language={className && className.replace('language-', '')}
        code={children}
        {...props}
      />
    </div>
  ),
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

export default ({ data, location }) => {
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

  return (
    <ThemeProvider>
      <SyntaxProvider>
        <GlobalStyles />
        <MdxStyles />
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
        <AppWrapper>
          <ContentContainer>
            <NavBarContainer>
              <NavBar />
            </NavBarContainer>
            <SidebarContainer isOpen={isOpen}>
              <Sidebar location={location} />
            </SidebarContainer>
            <Main>
              <Container>
                <PageTitle>{mdx.fields.title}</PageTitle>
                <GithubEditButton
                  link={`${docsLocation}/${mdx.parent.relativePath}`}
                />
                <div
                  className={css`
                    display: flex;
                    flex: 1;
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
              </Container>
            </Main>
            <MenuButtonContainer>
              <MenuButton
                onClick={() => setIsOpen(open => !open)}
                isOpen={isOpen}
              />
            </MenuButtonContainer>
          </ContentContainer>
        </AppWrapper>
      </SyntaxProvider>
    </ThemeProvider>
  );
};
