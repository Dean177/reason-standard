import styled, {
  createGlobalStyle,
  ThemeProvider as StyledThemeProvider,
} from 'styled-components';
import React, { useEffect, useState } from 'react';

export let colors = {
  purple: {
    base: '#372476',
    dark: '#3b173b',
  },
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
  },
};

export let fonts = {
  body: 'Roboto, Helvetica, Arial, sans-serif',
  monospace: `'Roboto Mono', monospace`,
};

export let spacing = {};

export let breakpoints = {
  desktop: 768,
};

export let dimensions = {
  maxContentWidth: 970,
  leftSideBar: 298,
  rightSideBar: 224,
};

export let themes = {
  light: {
    body: colors.grey.light,
    text: colors.black,
    toggle: {
      background: colors.grey.lightest,
      border: colors.white,
    },
  },
  dark: {
    body: colors.grey.dark,
    text: colors.white,
    toggle: {
      background: colors.grey.darkest,
      border: colors.black,
    },
  },
};

let ThemeContext = React.createContext(['light', () => {}]);

export const ThemeProvider = ({ children }) => {
  let [themeName, setTheme] = useState(() => {
    return (
      window.localStorage.getItem('theme') ||
      (window.matchMedia &&
        window.matchMedia('(prefers-color-scheme: dark)').matches &&
        'dark') ||
      'light'
    );
  });

  let toggleTheme = () => {
    setTheme(current => {
      return current === 'light' ? 'dark' : 'light';
    });
  };

  useEffect(() => {
    window.localStorage.setItem('theme', themeName);
  }, [themeName]);

  return (
    <StyledThemeProvider
      theme={themeName === 'light' ? themes.light : themes.dark}
    >
      <ThemeContext.Provider value={[themeName, toggleTheme]}>
        {children}
      </ThemeContext.Provider>
    </StyledThemeProvider>
  );
};

export let useTheme = () => {
  return React.useContext(ThemeContext);
};

const ToggleContainer = styled.button`
  background: ${({ theme }) => theme.toggle.background};
  border: 2px solid ${({ theme }) => theme.toggle.border};
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
    /* TOOD linear is weird */
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
  let isLight = theme === 'light';
  return (
    <ToggleContainer onClick={toggleTheme} isLight={isLight}>
      <span>🌞</span>
      <span>🌛</span>
    </ToggleContainer>
  );
};

// TODO get rid of all the crap in here
export const GlobalStyles = createGlobalStyle`

@import url('https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap');
@import url('https://cdn.jsdelivr.net/gh/tonsky/FiraCode@1.207/distr/fira_code.css');

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

.pre {
  border: 0;
  background-color: rgb(245, 247, 249);
  font-size: 14px;
  margin: 0px;
  padding: 16px;
  overflow: auto;
}

.code {
  background: #f9f7fb;
  border: 1px solid #ede7f3;
  border-radius: 4px;
  padding: 2px 6px;
  font-size: 0.9375em;
}

code, .code { 
  font-family: 'Roboto Mono', monospace; 
}

.blockquote {
  background-color: ${colors.grey.lighter};
  border-left: 3px solid ${colors.grey.base};
  padding-left: 14px;  
}
`;
