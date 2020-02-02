import React from 'react'
import { graphql } from 'gatsby'
import { MDXProvider } from '@mdx-js/react';
import styled, {css, ThemeProvider} from 'styled-components'
import MDXRenderer from 'gatsby-plugin-mdx/mdx-renderer'
import { Link } from '../components/Link'
import { GitHub} from '../components/Icon';
import mdxComponents from '../components/mdx';
import {colors, useTheme, GlobalStyles, themes, ThemeToggle} from '../theme'

let SearchInput = styled.input`
  background-color: #FFF;
  border-width: 1px;
  border-radius: 10px;
  border-style: solid;
  padding: 5px;
`
let SearchContainer = styled.div`
  background-color: '#111';
`
const Sidebar = () => {
  let [search, setSearch] = React.useState("")
  return (
    <SearchContainer>
      <SearchInput
        value={search}
        placeholder="Search"
        onChange={e => setSearch(e.target.value)}
      />
    </SearchContainer>
  );
}

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        headerTitle
        githubUrl
        title
        docsLocation
      }
    }
  }
`;

const NavBar = ({ githubUrl, themeName, toggleTheme }) => {
  return (
    <nav
      css={css`
        align-items: center;
        background-color: ${colors.red.dark};
        display: flex;
        flex-direction: row;
        justify-content: space-between;
        padding-top: 12px;
        padding-bottom: 12px;
        padding-left:15px;
        position: relative;
        z-index: 1;

        .navBarHeader {
          display: flex;
          align-items: center;
          color: #fff;
          font-size: 18px;
          font-weight: 500;
          height: auto;
          line-height: 1.5;
          &:hover {
            text-decoration: none;
            opacity: 0.8;
          }
        }

        .navLinks {
          align-items: center;
          display: flex;
          flex-direction: row;

          li {
            list-style-type: none;

            a {
              color: #fff;
              font-size: 16px;
              font-weight: 500;
              line-height: 1em;
              opacity: 1;
              padding: 10px 15px;

              &:hover {
                opacity: 0.7;
              }
            }
          }
        }
      `}
    >
      <Link to={'/'} className="navBarHeader">
        Standard
      </Link>
      <ul className="navLinks">
        <li>
          <ThemeToggle theme={themeName} toggleTheme={toggleTheme} />
        </li>
        <li>
          <Link to="/">docs</Link>
        </li>
        <li>
          <Link to="/api">api</Link>
        </li>
        <li>
          <Link
            to={githubUrl}
            css={css`
              display: flex;
              align-items: center;

              svg {
                fill: white;
                width: 15px;
                margin-right: 5px;
              }
            `}
          >
            <GitHub />
            <span>github</span>
          </Link>
        </li>
      </ul>
    </nav>
  );
};

export default ({ data }) => {
  let [isOpen, setIsOpen] = React.useState(false);
  let [themeName, toggleTheme] = useTheme()
  const {
    site: {
      siteMetadata: {
        githubUrl,
      },
    },
  } = data

  return (
    <ThemeProvider theme={themeName === 'light' ? themes.light : themes.dark}>
      <GlobalStyles />
      <div
        css={css`
          background-color: orange;
          display: flex;
          flex: 1;
          flex-direction: column;
          /* max-height: 100%; */
          width: 100%;
          position: relative;
        `}
      >
        <NavBar {...{ themeName, toggleTheme, githubUrl }} />
        <main
          css={css`
            background-color: lightseagreen;
            display: flex;
            flex: 1;
            flex-direction: column;
            overflow: auto;
            padding: 0px 22px;
            padding-top: 3rem;
          `}
        >
          <div
            css={css`
              display: flex;
              align-items: center;
              border-bottom: 1px solid ${colors.grey.light};
              padding-bottom: 40px;
              margin-bottom: 32px;

              h1 {
                border-left: 2px solid ${colors.red.dark};
                flex: 1;
                font-size: 32px;
                font-weight: 500;
                line-height: 1.5;
                margin-top: 0;
                padding: 0 16px;
                padding-top: 0;
              }
            `}
          >
            <h1>API</h1>
          </div>
          <div
            css={css`
              max-width: 750px;
            `}
          />
          <div
            css={css`
              height: 300px;
              background-color: red;
            `}
          />
          <div
            css={css`
              height: 400px;
              background-color: purple;
            `}
          />
          <div
            css={css`
              height: 500px;
              background-color: blue;
            `}
          />
          <MDXProvider components={mdxComponents}>
            {/* <MDXRenderer></MDXRenderer> */}
          </MDXProvider>
        </main>
        <div
          css={css`
            display: flex;
            flex: 1;
            flex-direction: column;
            position: absolute;
            background-color: lightblue;
          `}
        >
          <Sidebar />
        </div>
        <div
          onClick={() => setIsOpen(open => !open)}
          css={css`
            background-color: hotpink;
            position: absolute;
            bottom: 20px;
            right: 20px;
          `}
        >
          <span
            css={css`
              align-items: center;
              background-color: ${colors.white};
              border: 1px solid ${colors.red.dark};
              border-radius: 4px;
              display: flex;
              flex-direction: column;
              width: 36px;
              height: 33px;
              padding: 8px 5px;

              &:focus,
              &:hover {
                background-color: #542683;
              }

              .bar {
                display: block;
                width: 20px;
                height: 2px;
                border-radius: 1px;
                margin: 0 auto;
                margin-top: 4px;
                background-color: ${colors.red.dark};

                &:first-child {
                  margin-top: 0px;
                }
              }

              .cross {
                color: ${colors.red.dark};
              }
            `}
          >
            {isOpen ? (
              <>
                <span className="bar" />
                <span className="bar" />
                <span className="bar" />
              </>
            ) : (
              <span className="cross">╳</span>
            )}
          </span>
        </div>
      </div>
    </ThemeProvider>
  );
    }
    
    
