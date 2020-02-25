import React, { useState } from 'react';
import { useStaticQuery, graphql } from 'gatsby';
import _ from 'lodash';
import styled, { createGlobalStyle } from 'styled-components';
import { colors, dimensions } from '../theme';
import { Link } from './Link';

let TreeStyles = createGlobalStyle`
.showFrontLine .item >a:hover {
  background-color: #542683;
}
.active >a {
  background-color: ${colors.red.dark};
  color: ${colors.white};
}

.item .active > a {
  border-color: ${colors.red.dark};
  border-style: solid none solid solid;
  border-width: 1px 0px 1px 1px;
  background-color: ${colors.red.base};
  color: ${colors.white};
}
`;

const TreeNode = ({
  active,
  title,
  url,
  items,
}) => {
  const hasChildren = items.length !== 0;
  return (
    <li className={`item ${active ? 'active' : ''}`}>
      {title && <Link to={url}>{title}</Link>}
      {hasChildren ? (
        <ul
          className={'sideBarUL'}
          css={css`
            border-left: 1px solid #e6ecf1;
          `}
        >
          {items.map(item => (
            <TreeNode key={item.url} {...item} />
          ))}
        </ul>
      ) : null}
    </li>
  );
};

const SidebarContainer = styled('aside')`
  background-color: ${({ theme }) => theme.card.background};
  color: ${({ theme }) => theme.card.text};
  margin-left: -1px;
  padding-left: 1px;
  padding-top: 32px;
  padding-bottom: 32px;
  width: 100%;
  height: 100vh;
  overflow: auto;
  top: 0;

  .sideBarUL {
    padding: 0;
    list-style: none;
  }

  .sideBarUL li a {
    color: ${({ theme }) => theme.card.text};
    font-size: 14px;
    font-weight: 500;
    line-height: 1.5;
    padding: 9px 27px 9px 27px;
  }

  .sideBarUL .item > a {
    display: flex;
    align-items: center;
    position: relative;
    width: 100%;
    &:hover {
      background-color: ${colors.red.base};
      color: #fff;
    }
  }

  .sideBarUL .item .item {
    margin-left: 16px;
  }

  @media only screen and (min-width: 767px) {
    height: 100vh;
    width: ${dimensions.leftSidebarWidth}px;
  }
`;

let generateItems = (baseUrl, items = []) =>
  items.map(item => {
    let url = `${baseUrl}${item.url}`;
    return {
      title: item.title,
      url,
      items: generateItems(url, item.items),
    };
  });

export const Sidebar = ({location}) => {
  const { allMdx } = useStaticQuery(graphql`
    query {
      allMdx {
        edges {
          node {
            fields {
              url
              title
            }
            tableOfContents
            frontmatter {
              order
            }
          }
        }
      }
    }
  `);
  let pages = _.sortBy(allMdx.edges, edge =>
    Number(edge.node.frontmatter.order || '999'),
  );
  const tree = pages.map(({ node: { fields, tableOfContents } }) => {
    let active = fields.url === location.pathname
    return {
      active,
      title: fields.title,
      url: fields.url,
      items: active ? generateItems(fields.url, tableOfContents.items) : [],
    }
  });
  return (
    <SidebarContainer>
      <TreeStyles />
      <ul className={'sideBarUL'}>
        {tree.map(treeData => (
          <TreeNode {...treeData} />
        ))}
      </ul>
    </SidebarContainer>
  );
};
