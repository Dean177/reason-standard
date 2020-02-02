import styled,{ createGlobalStyle } from 'styled-components'
import React, {useEffect,useState} from 'react';

export let colors = {
  red: {
    lightest: 'rgb(245,221,218)',
    light: 'rgba(224, 96, 80, 0.694118)',
    base: 'rgb(194, 52, 38)',
    dark: 'rgb(219, 77, 63)',
    darkest: 'rgb(162,70,57)',
  },
  black: 'rgb(0,0,0)',
  grey: {
    lighter: 'rgb(230, 236, 241)',
    light: 'rgb(246,244,244)',
    base: 'rgb(113,113,113)',
    dark: 'rgb(55,55,55)',
  },
  white: 'rgb(255,255,255)',
  yellow: {
    base: 'rgb(251,230,121)',
    light: 'rbg(254,247,215)',
  }
}

export let fonts = {
  body: 'BlinkMacSystemFont, -apple-system, "Segoe UI", Roboto, Helvetica, Arial, sans-serif',
  mono: '"SF Mono", "Roboto Mono", Menlo, monospace'
}

export let themes = {
  light: {
    body: colors.grey.light,
    text: colors.black,
  },
  dark: {
    body: colors.grey.dark,
    text: colors.white,
  },
};

export let useTheme = () => {
  let [themeName, setTheme] = useState(() => {
    return (
      window.localStorage.getItem('theme') ||
      (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches && 'dark') ||
      'light'
    );
    }
  )
  let toggleTheme = () => {
    setTheme(current => {
      return current === 'light' ? 'dark' : 'light';
    });
  };
  useEffect(() => {
    window.localStorage.setItem('theme', themeName);
  }, [themeName]);
  return [themeName, toggleTheme];
};

const ToggleContainer = styled.button`
  background: ${({ theme }) => theme.gradient};
  border: 2px solid ${({ theme }) => theme.toggleBorder};
  border-radius: 30px;
  cursor: pointer;
  display: flex;
  font-size: 0.5rem;
  justify-content: space-between;
  margin: 0 auto;
  overflow: hidden;
  padding: 0.4rem;
  position: relative;
  width: 4rem;
  height: 2rem;

  span {
    font-size: 1rem;
    width: 2.5rem;
    transition: all 0.3s linear;

    // sun icon
    &:first-child {
      transform: ${({ isLight }) =>
        isLight ? 'translateY(0)' : 'translateY(60px)'};
    }

    // moon icon
    &:nth-child(2) {
      transform: ${({ isLight }) =>
        isLight ? 'translateY(-60px)' : 'translateY(0)'};
    }
  }
`;

export const ThemeToggle = ({ theme, toggleTheme }) => {
  let isLight = theme === 'light'
  return (
    <ToggleContainer onClick={toggleTheme} isLight={isLight}>
      <span>🌞</span>
      <span>🌛</span>
    </ToggleContainer>
  );
};

export const GlobalStyles = createGlobalStyle`

@import url('https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap');

*,
*::after,
*::before {
  box-sizing: border-box;
}

* {
  margin: 0;
  padding: 0;
}

html, body {
  background: ${({ theme }) => theme.body};
  color: ${({ theme }) => theme.text};
  font-family: BlinkMacSystemFont, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
  font-size: 16px;
}

#___gatsby, #gatsby-focus-wrapper {
  display: flex;
  flex:1;
  height: 100vh;    
}
 
a {
  color: ${colors.red.base};
  text-decoration: none;
}
a:hover {
  text-decoration: none;
}

.heading1 {
  font-size: 26px;
  font-weight: 800;
  line-height: 1.5;
  margin-bottom: 16px;
  margin-top: 32px;
}

.heading2 {
  font-size: 24px;
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
  margin: 16px 0px 32px;
  line-height: 1.625;
}

pre {
  border: 0;
  background-color: rgb(245, 247, 249); /*; */
}

.mainWrapper ul, .mainWrapper ol {
  margin: 24px 0px;
  padding: 0px 0px 0px 2em;
}

.mainWrapper ul li, .mainWrapper ol li {
  font-size: 16px;
  line-height: 1.8;
  font-weight: 400;
}

/* Header section ends here */
.headerNav {
  font-family: 'Roboto';
  padding: 0px 24px;
  color: rgb(194, 52, 38);
  font-size: 16px;
  font-weight: 500;
  line-height: 1em;
}

.headerNav a {
  color: rgb(194, 52, 38);
  text-decoration: none;
}

.headerNav a:hover {
  text-decoration: none;
}

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

.rightSideTitle {
  font-size: 10px;
  line-height: 1;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.2px;
  padding: 7px 24px 7px 16px;
  border-left: 1px solid #e6ecf1;
  border-left-color: rgb(230, 236, 241);
  color: #4C5767;
}

.rightSideBarUL {
  margin-top: 32px;
}

.rightSideBarUL li {
  list-style-type: none;
  border-left: 1px solid #e6ecf1;
  border-left-color: rgb(230, 236, 241);
}

.rightSideBarUL li a {
  font-size: 12px;
  font-weight: 500;
  line-height: 1.5;
  padding: 7px 24px 7px 16px;
}

.hideFrontLine .collapser {
  background: transparent;
  border: none;
  outline: none;
  position: absolute;
  right: 20px;
  z-index: 1;
  cursor: pointer;
}

.hideFrontLine  .active >a{
  background-color: #542683;
  color: #fff;
}
.firstLevel ul li .collapser svg path {
  fill: #fff;
}
.active .collapser >svg >path {
  fill: #663399;
}

.firstLevel ul .item ul .item {
  border-left: 1px solid #e6ecf1;
}

.sideBarUL .item{
  list-style: none;
  padding: 0;
}
.sideBarUL .item >a {
  color: #fff;
  text-decoration: none;
  display: flex;
  align-items: center;
  position: relative;
  width: 100%;
  padding-right: 35px;
  padding-left: 15px;
}
.sideBarUL .item>a:hover {
  background-color: #542683;
  color: #fff;
}
.showFrontLine .item >a:hover {
  background-color: #542683;
}
.showFrontLine .active >a {
  color: #fff;
  background-color: #473485;
}

.sideBarUL  .item .item{
  margin-left: 16px;
}

.firstLevel>ul>.item{
  margin-left: 0;
}

.showFrontLine  .item .item {
  border-left: 1px solid #e6ecf1;
  border-left-color: rgb(230, 236, 241);
  padding: 0;
  width: calc(100% - 16px);
}

.showFrontLine .item .active>a {
  border-color: rgb(230,236,241);
  border-style: solid none solid solid;
  border-width: 1px 0px 1px 1px;
  background-color: #542683;
  color: #fff;
}

.titleWrapper {
  
}

.title {
  
}

.addPaddTopBottom {
  padding: 50px 0;
}

.nextPreviousWrapper {
  margin: 0px;
  padding: 0px;
  width: auto;
  display: grid;
  grid-template-rows: auto;
  column-gap: 24px;
  grid-template-columns: calc(50% - 8px) calc(50% - 8px);
}

.previousBtn {
  cursor: pointer;  
  margin: 0px;
  padding: 0px;
  position: relative;
  display: flex;
  flex-direction: row;
  align-items: center;
  place-self: stretch;
  color: rgb(36, 42, 49);
  background-color: rgb(255, 255, 255);
  border-radius: 3px;
  border: 1px solid rgb(230, 236, 241);
  transition: border 200ms ease 0s;
  box-shadow: rgba(116, 129, 141, 0.1) 0px 3px 8px 0px;
  text-decoration: none;
}

.leftArrow {
  display: block;
  margin: 0px;
  color: rgb(157, 170, 182);
  flex: 0 0 auto;
  font-size: 24px;
  transition: color 200ms ease 0s;
  padding: 16px;
  padding-right: 16px;
}

.preRightWrapper {
  display: block;
  margin: 0px;
  flex: 1 1 0%;
  padding: 16px;
  text-align: right;
}

.smallContent {
  display: block;
  margin: 0px;
  padding: 0px;
  color: #6E6E6E;
}

.smallContent span {
  font-size: 12px;
  line-height: 1.625;
  font-weight: 400;
}

.nextPreviousTitle {
  display: block;
  margin: 0px;
  padding: 0px;
  transition: color 200ms ease 0s;
}

.nextPreviousTitle span {
  font-size: 16px;
  line-height: 1.5;
  font-weight: 500;
}

.nextBtn {
  cursor: pointer;
  -moz-box-align: center;
  -moz-box-direction: normal;
  -moz-box-orient: horizontal;
  margin: 0px;
  padding: 0px;
  position: relative;
  display: flex;
  flex-direction: row;
  align-items: center;
  place-self: stretch;
  color: rgb(36, 42, 49);
  background-color: rgb(255, 255, 255);
  border-radius: 3px;
  border: 1px solid rgb(230, 236, 241);
  transition: border 200ms ease 0s;
  box-shadow: rgba(116, 129, 141, 0.1) 0px 3px 8px 0px;
  text-decoration: none;
}

.rightArrow {
  flex: 0 0 auto;
  font-size: 24px;
  transition: color 200ms ease 0s;
  padding: 16px;
  padding-left: 16px;
  display: block;
  margin: 0px;
  color: rgb(157, 170, 182);
}

.nextRightWrapper {
  display: block;
  margin: 0px;
  padding: 16px;
  flex: 1 1 0%;
}

.nextBtn:hover, .previousBtn:hover {
  color: rgb(116, 76, 188);
  text-decoration: none;
  border: 1px solid rgb(116, 76, 188);
}

.nextBtn:hover .rightArrow, .previousBtn:hover .leftArrow {
  color: rgb(116, 76, 188);
}


.pre {
  font-size: 14px;
  margin: 0px;
  padding: 16px;
  overflow: auto;
}

.mainWrapper code {
  background: #f9f7fb;
  border: 1px solid #ede7f3;
  border-radius: 4px;
  padding: 2px 6px;
  font-size: 0.9375em;
}


`;