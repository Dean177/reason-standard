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
  border-color: ${colors.grey.base};
  border-radius: 10px;
  border-style: solid;
  border-width: 1px;
  font-size: 16px;
  padding: 8px;
`
let SearchContainer = styled.div`
  background-color: ${colors.grey.light};  
  padding: 15px;
  display: flex;
  flex-direction: column;
  flex-shrink: 0;
`;
const Sidebar = () => {
  let [search, setSearch] = React.useState("")
  return (
    <div
      css={css`
        background-color: ${colors.white};
        display: flex;
        flex-direction: column;
        height: 100%;
        overflow: hidden;
        width: 100%;
      `}
    >
      <SearchContainer>
        <SearchInput
          value={search}
          placeholder="Search"
          onChange={e => setSearch(e.target.value)}
        />
      </SearchContainer>
      <div
        css={css`
          background-color: lightgoldenrodyellow;
          display: flex;
          flex-direction: column;
          overflow: auto;
        `}
      >
        {Array(100)
          .fill('ModuleName')
          .map((s, index) => (
            <div
              key={index}
              css={css`
                padding: 50px;
              `}
            >
              {s}
            </div>
          ))}
      </div>
    </div>
  );
}

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        githubUrl
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
        flex-shrink: 0;
        flex-direction: row;
        justify-content: space-between;
        padding-top: 12px;
        padding-bottom: 12px;
        padding-left: 15px;
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
          display: flex;
          height: 100%;
          overflow: hidden;
          position: relative;
          width: 100%;
        `}
      >
        <div
          css={css`
            background-color: lightgoldenrodyellow;
            display: flex;
            flex-direction: column;
            overflow: auto;
            height: 100%;
            width: 100%;
          `}
        >
          <NavBar {...{ themeName, toggleTheme, githubUrl }} />
          <main
            css={css`
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
                min-height: 300px;
                background-color: red;
              `}
            />
            <div
              css={css`
                min-height: 400px;
                background-color: purple;
              `}
            />
            <div
              css={css`
                min-height: 500px;
                background-color: blue;
              `}
            />
            <MDXProvider components={mdxComponents}>
              {/* <MDXRenderer></MDXRenderer> */}
            </MDXProvider>
          </main>
        </div>
        <div
          css={css`
            background-color: lightblue;
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
            z-index: 3;
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

              &:focus,
              &:hover {
                .bar {
                  background-color: ${colors.red.base};
                }
                .cross {
                  color: ${colors.red.base};
                }
              }
            `}
          >
            {isOpen ? (
              <span className="cross">╳</span>
            ) : (
              <span className="cross">🔍</span>
            )}
          </span>
        </div>
      </div>
    </ThemeProvider>
  );
    }
    
    
