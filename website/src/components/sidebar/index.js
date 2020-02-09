import React from "react";
import {Tree} from './tree';
import {StaticQuery, graphql} from "gatsby";
import styled from 'styled-components'
import { colors, dimensions } from "../../theme";

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

const Sidebar = styled('aside')`
  background-color: ${colors.purple.base};
  background: linear-gradient(${colors.purple.base}, ${colors.purple.dark});

  width: 100%;
  height: 100vh;
  overflow: auto;
  /* position: fixed; */
  /* position: sticky; */
  top: 0;

  .sideBarUL {
    margin-top: 32px;
  }

  .sideBarUL li {
    list-style-type: none;
    width: auto;
  }

  .sideBarUL li a {
    color: #fff;
    font-size: 14px;
    font-weight: 500;
    line-height: 1.5;
    padding: 7px 24px 7px 16px;
    padding-left: 10px;
    padding-right: 25px;
    border-style: solid none solid solid;
    border-width: 1px 0px 1px 1px;
    border-color: transparent currentcolor transparent transparent;
  }

  .sideBarUL .item {
    list-style: none;
    padding: 0;
  }

  .sideBarUL .item > a {
    color: #fff;
    text-decoration: none;
    display: flex;
    align-items: center;
    position: relative;
    width: 100%;
    padding-right: 35px;
    padding-left: 15px;
    &:hover {
      background-color: #542683;
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


const Divider = styled(props => (
  <li {...props}>
    <hr />
  </li>
))`
  list-style: none;
  padding: 0.5rem 0;

  hr {
    margin: 0;
    padding: 0;
    border: 0;
    border-bottom: 1px solid #ede7f3;
  }
`;


export const LeftSidebar = ({location}) => (
  <StaticQuery
    query={graphql`
      query {
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
    `}
    render={({allMdx}) => {
      return (
        <Sidebar>
          <ul className={'sideBarUL'}>
            <Tree
              edges={allMdx.edges}
            />          
          </ul>
        </Sidebar>
      );
    }}
  />
);
