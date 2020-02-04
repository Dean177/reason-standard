import React from 'react'
import { graphql } from 'gatsby'
import styled, {css} from 'styled-components'
import { breakpoints, colors, GlobalStyles, ThemeProvider } from '../theme'
import { MenuButton, NavBar, ContentContainer, AppContainer, PageTitle } from '../components/Layout' 

let SearchInput = styled.input`
  background-color: #fff;
  border-color: ${colors.grey.light};
  border-radius: 10px;
  border-style: solid;
  border-width: 1px;
  font-size: 16px;
  padding: 10px 12px;
`;
let SearchContainer = styled.div`
  background-color: ${colors.grey.lighter};  
  padding: 12px;
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
                padding-top: 50px;
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

export default ({ data }) => {
  let [isOpen, setIsOpen] = React.useState(false);
  const {
    site: {
      siteMetadata: {
        githubUrl,
      },
    },
  } = data

  return (
    <ThemeProvider>
      <GlobalStyles />
      <AppContainer>
        <ContentContainer>
          <NavBar githubUrl={githubUrl} />
          <div
            css={css`
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

              @media (min-width: ${breakpoints.desktop}px) {
                display: none;
              }
            `}
          >
            <Sidebar />
          </div>
          <main
            css={css`
              align-self: center;
              display: flex;
              flex: 1;              
              flex-direction: column;
              max-width: 970px;
              width: 100%;
              padding: 0px 22px;
              padding-top: 3rem;
            `}
          >
            <PageTitle>API</PageTitle>
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
          </main>
        </ContentContainer>

        <div
          css={css`
            position: absolute;
            bottom: 24px;
            right: 24px;
            z-index: 3;

            @media (min-width: ${breakpoints.desktop}px) {
              display: none;
            }
          `}
        >
          <MenuButton
            onClick={() => setIsOpen(open => !open)}
            isOpen={isOpen}
          />
        </div>
      </AppContainer>
    </ThemeProvider>
  );
}

