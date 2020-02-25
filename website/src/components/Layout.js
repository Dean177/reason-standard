import { useStaticQuery, graphql } from 'gatsby';
import React from 'react'
import styled from 'styled-components';
import { dimensions, colors, spacing, useTheme, ThemeToggle } from '../theme'
import { Link } from './Link';
import { GitHub } from './Icon';

export const AppContainer = styled.div`
  display: flex;
  height: 100%;
  overflow: hidden;
  position: relative;
  width: 100%;
`;

export const ContentContainer = styled.div`  
  overflow: auto;
  height: 100%;
  width: 100%;
`;

export const NavBar = () => {
  let [themeName, toggleTheme] = useTheme();
  let { site: { siteMetadata: { githubUrl } } } = useStaticQuery(graphql`
    query {
      site {
        siteMetadata {
          githubUrl
        }
      }
    }`
  )

  return (
    <nav
      css={css`
        background-color: ${({ theme }) => theme.navbar.background};
        display: flex;
        flex-direction: column;
        min-height: ${dimensions.navbar}px;
        width: 100%;
        padding-left: ${spacing.medium}px;
        padding-right: ${spacing.medium}px;
      `}
    >
      <div
        css={css`
          align-items: center;
          align-self: center;
          display: flex;
          flex-direction: row;
          flex-shrink: 0;
          justify-content: space-between;
          max-width: ${dimensions.maxContentWidth}px;
          padding-top: 12px;
          padding-bottom: 12px;
          width: 100%;
          z-index: 1;

          .navBarHeader {
            align-items: center;
            display: flex;
            color: ${({ theme }) => theme.navbar.text};
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

            .navLink {
              a {
                color: ${({ theme }) => theme.navbar.text};
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
        <div className="navLinks">
          <div className="navLink">
            <ThemeToggle theme={themeName} toggleTheme={toggleTheme} />
          </div>
          <div className="navLink">
            <Link to="/docs">docs</Link>
          </div>
          <div className="navLink">
            <Link to="/api">api</Link>
          </div>
          {/* TODO get the playground working */}
          {/* <div>
            <Link to="/try">try</Link>
          </div> */}
          <div className="navLink">
            <Link
              to={githubUrl}
              css={css`
                display: flex;
                align-items: center;

                svg {
                  fill: ${({ theme }) => theme.navbar.text};
                  width: 15px;
                  margin-right: 5px;
                }
              `}
            >
              <GitHub />
              <span>github</span>
            </Link>
          </div>
        </div>
      </div>
    </nav>
  );
};

export let PageTitle = ({children}) => {
  return (
    <div
      css={css`
        display: flex;
        align-items: center;
        padding-bottom: 40px;

        h1 {
          border-left: 2px solid ${colors.red.dark};
          flex: 1;
          font-size: 32px;
          font-weight: 500;
          line-height: 1.5;
          margin-top: 0;
          padding: 0 16px;
        }
      `}
    >
      <h1>{children}</h1>
    </div>
  );
}

export const MenuButton = ({ onClick, isOpen }) => {
  return (
      <div
        onClick={onClick}
        css={css`
          align-items: center;
          background-color: ${colors.red.dark};
          border: 1px solid ${colors.white};
          border-radius: 4px;
          color: ${colors.white};
          display: flex;
          flex-direction: column;
          width: 38px;
          height: 38px;
          padding: 9px 6px;

          &:focus,
          &:hover {
            background-color: ${colors.red.base};
          }
        `}
      >
        <span>{isOpen ? '╳' : '⠇'}</span>
      </div>
    
  );
};
    