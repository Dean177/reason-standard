import React from "react";
import Helmet from "react-helmet";
import { graphql } from "gatsby";
import MDXRenderer from "gatsby-plugin-mdx/mdx-renderer";
import { MDXProvider } from '@mdx-js/react';
import { LeftSideBarWidth, Content, Wrapper, RightSideBarWidth } from "../components/Layout";
import { LeftSidebar } from "../components/sidebar";
import { RightSidebar } from "../components/RightSideBar";
import { NextPrevious } from '../components/NextPrevious';
import mdxComponents from '../components/mdx';
import { GithubEditButton } from '../components/GithubEditButton';

export const pageQuery = graphql`
  query($id: String!) {
    site {
      siteMetadata {
        title
        docsLocation
        favicon
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

const DocTemplate = (props) => {
    const { data } = props;
    const {
      allMdx,
      mdx,
      site: {
        siteMetadata: { docsLocation }
      }
    } = data;
    

    const navItems = allMdx.edges
      .map(({ node }) => node.fields.slug)
      .filter(slug => slug !== "/")
      .sort()
      .reduce(
        (acc, cur) => {
          return {
            ...acc,
            items: [...acc.items, cur]
          };
        },
        { items: [] }
      );

    const nav = []
      .reduce((acc, cur) => {
        return acc.concat(navItems[cur]);
      }, [])
      .concat(navItems.items)
      .map(slug => {
        if(slug) {
          const { node } = allMdx.edges.find(
            ({ node }) => node.fields.slug === slug
          );

          return { title: node.fields.title, url: node.fields.slug };
        }
      });

    const title = mdx.frontmatter.metaTitle;
    const description = mdx.frontmatter.metaDescription;
    // TODO static query
    // let canonicalUrl = `${config.gatsby.pathPrefix}${config.gatsby.siteUrl}${mdx.fields.slug}`;

    return (
      <MDXProvider components={mdxComponents}>
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
            <meta name="description" content={description} />
          ) : null}
          {description ? (
            <meta property="og:description" content={description} />
          ) : null}
          {description ? (
            <meta property="twitter:description" content={description} />
          ) : null}
          {/* TODO Static query<link
            rel="shortcut icon"
            type="image/svg"
            href={config.siteMetadata.favicon}
          /> 
          <link rel="canonical" href={canonicalUrl} />*/}
        </Helmet>

        <Wrapper>
          <LeftSideBarWidth className={'hiddenMobile'}>
            <LeftSidebar location={location} />
          </LeftSideBarWidth>
          <Content>
            <div className={'titleWrapper'}>
              <h1 className={'title'}>{mdx.fields.title}</h1>
              <GithubEditButton
                link={`${docsLocation}/${mdx.parent.relativePath}`}
              />
            </div>
            <div className={'mainWrapper'}>
              <MDXRenderer>{mdx.body}</MDXRenderer>
            </div>
            <div className={'addPaddTopBottom'}>
              <NextPrevious mdx={mdx} nav={nav} />
            </div>
            <RightSideBarWidth>
              <RightSidebar location={location} />
            </RightSideBarWidth>
          </Content>
        </Wrapper>
      </MDXProvider>
    );
}


export default DocTemplate
