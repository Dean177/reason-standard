import React from 'react'
import styled from 'styled-components';
import { dimensions, colors, useTheme, ThemeToggle } from '../theme'
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
  display: flex;
  flex-direction: column;
  overflow: auto;
  height: 100%;
  width: 100%;
`;

export const NavBar = ({ githubUrl }) => {
  let [themeName, toggleTheme] = useTheme();

  return (
    <nav
      css={css`
        background-color: ${colors.red.dark};
        display: flex;
        flex-direction: column;
        width: 100%;
      `}
    >
      <div
        css={css`
          align-items: center;
          align-self: center;
          display: flex;
          flex-shrink: 0;
          flex-direction: row;
          justify-content: space-between;
          max-width: ${dimensions.maxContentWidth}px;
          padding-top: 12px;
          padding-bottom: 12px;
          padding-left: 15px;
          width: 100%;
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
    