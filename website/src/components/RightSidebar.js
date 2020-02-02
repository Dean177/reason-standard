import React from "react";
import { StaticQuery, graphql } from "gatsby";
import styled from 'styled-components'

const RightSidebarContainer = styled('aside')`
  width: 100%;
  background-color: #fff;
  border-right: 1px solid #ede7f3;
  height: 100vh;
  overflow: auto;
  position: fixed;
  padding-left: 24px;
  position: -webkit-sticky;
  position: -moz-sticky;
  position: sticky;
  top: 0;
  @media only screen and (max-width: 50rem) {
    width: 100%;
    position: relative;
  }
`;

// eslint-disable-next-line no-unused-vars
const ListItem = styled(({ className, active, level, ...props }) => {
    return (
      <li className={className}>
        <a href={props.to} {...props} />
      </li>
    );
})`
  list-style: none;

  a {
    color: #5C6975;
    text-decoration: none;
    font-weight: ${({ level }) => (level === 0 ? 700 : 400)};
    padding: 0.45rem 0 0.45rem ${props => 2 + (props.level || 0) * 1}rem;
    display: block;
    position: relative;

    &:hover {
      color: rgb(116, 76, 188) !important;
    }

    ${props =>
      props.active &&
      `
      color: #663399;
      border-color: rgb(230,236,241) !important;
      border-style: solid none solid solid;
      border-width: 1px 0px 1px 1px;
      background-color: #fff;
    `} // external link icon
    svg {
      float: right;
      margin-right: 1rem;
    }
  }
`;

export const RightSidebar = ({ location }) => (
  <StaticQuery
    query={graphql`
      query {        
        allMdx {
          edges {
            node {
              fields {
                slug
              }
              tableOfContents
            }
          }
        }
      }
    `}
    render={({ allMdx }) => {
      if (allMdx.edges.length === 0) {
        return (
          <RightSidebarContainer>
            <ul></ul>
          </RightSidebarContainer>
        );
      }

      return (
        <RightSidebarContainer>
          <ul className={"rightSideBarUL"}>
            <li className={"rightSideTitle"}>CONTENTS</li>
            {allMdx.edges.map((item) => {
              return item.node.tableOfContents.items.map((innerItem, index) => {
                return (
                  <ListItem
                    key={index}
                    to={`#${innerItem.title ? innerItem.title.replace(/\s+/g, '').toLowerCase() : ''}`}
                    level={1}
                  >
                    {innerItem.title}
                  </ListItem>
                );
              })
            })}
          </ul>
        </RightSidebarContainer>
      );
    }}
  />
);
