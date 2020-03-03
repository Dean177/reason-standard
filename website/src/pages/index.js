import React, { useEffect } from 'react';
import { graphql } from 'gatsby';
import { useSpring, animated } from 'react-spring';
import styled, { css } from 'styled-components';
import {
  breakpoints,
  colors,
  GlobalStyles,
  ThemeProvider,
} from '../theme';
import {
  NavBar,
  ContentContainer,
  AppContainer,
  Main,
  NavBarContainer,
} from '../components/Layout';
import { CodeBlock } from '../components/CodeBlock';
import { Ocaml, Reason } from '../components/Icon';
import { SyntaxProvider } from '../components/Syntax';

let AnimatedOcaml = animated(Ocaml);
let AnimatedReason = animated(Reason);

import {
  ArtificialInteligence,
  BookLover,
  Productive,
} from '../components/Illustration';

let logoSize = 180;

const SellingPoint = ({ description, illustration, flip }) => {
  return (
    <div
      css={css`
        display: flex;
        flex-direction: column;
        padding-bottom: 50px;

        .illustration-container {
          align-items: center;
          /* background-color: lightgoldenrodyellow; */
          display: flex;
          flex: 1;
          flex-direction: column;

          .illustration {
            height: 200px;
            padding: 30px;
          }
        }

        .description {
          background-color: ${({ theme }) => theme.card.background};
          display: flex;
          flex: 1;
          font-size: 20px;
          line-height: 1.4;
          padding: 40px;
          padding-top: 30px;
        }

        @media (min-width: ${breakpoints.desktop}px) {
          flex-direction: ${flip ? 'row-reverse' : 'row'};

          .illustration-container {
            align-items: ${flip ? 'flex-start' : 'flex-end'};
            display: flex;
            flex: 1;
            flex-direction: column;
          }
        }
      `}
    >
      <div className="illustration-container">{illustration()}</div>
      <div className="description">{description}</div>
    </div>
  );
};

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        docsLocation
      }
    }
    allMdx {
      edges {
        node {
          frontmatter {
            order
          }
          fields {
            url
            title
          }
        }
      }
    }
  }
`;

export default props => {
  let [logo, setLogo] = React.useState('reason');
  // useEffect(() => {
  //   let toggleLogo = setInterval(() => {
  //     setLogo(current => (current === 'reason' ? 'ocaml' : 'reason'));
  //   }, 3000);
  //   return () => {
  //     clearInterval(toggleLogo);
  //   };
  // });

  const logoStyles = useSpring(
    logo === 'reason'
      ? {
          background: `linear-gradient(${colors.red.base}, ${colors.red.base})`,
          borderRadius: 0,
          reTransform: `translate3d(${logoSize / 7}px,${-logoSize / 2.25}px,0)`,
          camlTransform: `translate3d(-${logoSize}px, ${-logoSize /
            1.12}px, 0)`,
        }
      : {
          background: `linear-gradient(${colors.orange.ocaml.start}, ${colors.orange.ocaml.end})`,
          borderRadius: 40,
          reTransform: `translate3d(${logoSize}px, ${-logoSize / 2.25}px, 0)`,
          camlTransform: `translate3d(${0}px, ${-logoSize / 1.12}px,0)`,
        },
  );
  return (
    <ThemeProvider>
      <SyntaxProvider>
        <GlobalStyles />
        <AppContainer>
          <ContentContainer>
            <NavBarContainer>
              <NavBar />
            </NavBarContainer>
            <div
              css={css`
                align-items: center;
                display: flex;
                flex-direction: column;
                width: 100%;
              `}
            >
            <Main>
                <h1
                  css={css`
                    padding-top: 20px;
                    padding-bottom: 20px;
                    font-size: 50px;
                    font-weight: normal;
                    letter-spacing: 1.1px;
                  `}
                >
                  Standard
                </h1>
                <p>A standard library replacement for Reason and Ocaml. </p>
                <p
                  css={css`
                    padding-top: 30px;
                    padding-bottom: 30px;
                  `}
                >
                  Standard provides an easy-to-use, comprehensive and performant
                  standard library, that has the same API for the OCaml and
                  Bucklescript compilers.
                </p>
                <div
                  css={css`
                    h1 {
                    }
                  `}
                >
                  <button>Get started</button>
                </div>
                <div
                  css={css`
                    padding-bottom: 50px;
                  `}
                >
                  <CodeBlock
                    language="reason"
                    code={`
open Standard;

String.toList("somestring")
->List.filterMap(~f=character => 
    Char.toCode(character)->Int.add(1)->Char.ofCode
  )
->String.ofList
/* "asdfasdf" */
                  `}
                  />
                </div>
                <SellingPoint
                  illustration={() => (
                    <animated.div
                      onClick={() =>
                        setLogo(current =>
                          current === 'ocaml' ? 'reason' : 'ocaml',
                        )
                      }
                      style={{
                        display: 'block',
                        flexShrink: 0,
                        width: logoSize,
                        height: logoSize,
                        background: logoStyles.background,
                        borderRadius: logoStyles.borderRadius,
                        overflow: 'hidden',
                        margin: 20,
                        position: 'relative',
                      }}
                    >
                      <AnimatedReason
                        style={{
                          transform: logoStyles.reTransform,
                          position: 'absolute',
                          fill: 'white',
                        }}
                        height={logoSize * 2}
                        width={logoSize * 2}
                      />
                      <AnimatedOcaml
                        style={{
                          transform: logoStyles.camlTransform,
                          position: 'absolute',
                          fill: 'white',
                        }}
                        height={logoSize * 3}
                        width={logoSize * 3}
                      />
                    </animated.div>
                  )}
                  description="Works with either the Reason or Ocaml syntax, targetting the bucklescript, native or js_of_ocaml compilers"
                />
                <SellingPoint
                  flip={true}
                  illustration={() => <BookLover className="illustration" />}
                  description={`
                  Easy to learn.
                  Excellent documentation 
                  Examples for each function
                  Thoroughly tested
                  `}
                />
                <SellingPoint
                  illustration={() => <Productive className="illustration" />}
                  description={`
                  Safe
                  Banish runtime errors.
                `}
                />
                <SellingPoint
                  flip={true}
                  illustration={() => (
                    <ArtificialInteligence className="illustration" />
                  )}
                  description={`
                  Advanced.
                  Index operators for Maps, Sets arrays and strings
                  (let+) Bindings for Options & Results
              `}
                />
            </Main>
            </div>
          </ContentContainer>
        </AppContainer>
      </SyntaxProvider>
    </ThemeProvider>
  );
};
