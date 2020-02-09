import React from 'react';
import { graphql } from 'gatsby';
import { useSpring, animated } from 'react-spring';
import styled, { css } from 'styled-components';
import { breakpoints, colors, GlobalStyles, ThemeProvider } from '../theme';
import {
  MenuButton,
  NavBar,
  ContentContainer,
  AppContainer,
  PageTitle,
} from '../components/Layout';
import { NextPrevious } from '../components/NextPrevious';
import { CodeBlock } from '../components/CodeBlock';

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
          background-color: white;
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
        githubUrl
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
  const {
    site: {
      siteMetadata: { githubUrl },
    },
    allMdx,
  } = props.data;
  let [logo, setLogo] = React.useState('reason');

  // Todo, use a static query to move this into NextPrevious
  const nav = allMdx.edges
    .filter(({ node }) => node.fields.url !== '/')
    .map(({ node }) => ({
      title: node.fields.title,
      url: node.fields.url,
      order: Number(node.frontmatter.order ||"999"),
    }))
    .sort((fieldsA, fieldsB) => fieldsA.order - fieldsB.order);

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
          background: 'linear-gradient(#f29100", #ec670f)',
          borderRadius: 40,
          reTransform: `translate3d(${logoSize}px, ${-logoSize / 2.25}px, 0)`,
          camlTransform: `translate3d(${0}px, ${-logoSize / 1.12}px,0)`,
        },
  );
  return (
    <ThemeProvider>
      <GlobalStyles />
      <AppContainer>
        <ContentContainer>
          <NavBar githubUrl={githubUrl} />
          <main
            css={css`
              align-items: center;
              display: flex;
              flex: 1;
              flex-direction: column;
              width: 100%;
              overflow: auto;
              padding: 0px 22px;
              padding-top: 3rem;
              padding-bottom: 3rem;
            `}
          >
            <div
              css={css`
                max-width: 970px;
              `}
            >
              <h1
                css={css`
                  padding-top: 20px;
                `}
              >
                Standard
              </h1>
              <p
                css={css`
                  padding-top: 30px;
                  padding-bottom: 30px;
                `}
              >
                A standard library replacement for Reason and Ocaml. Standard
                provides an easy-to-use, comprehensive and performant standard
                library, that has the same API for the OCaml and Bucklescript
                compilers.
              </p>
              <div
                css={css`
                  padding-bottom: 50px;
                `}
              >
                <CodeBlock langauge="reason">
                  {`
open Standard;

String.toList("somestring")
->List.filterMap(~f=character => 
  Char.toCode(character)->Int.add(1)->Char.ofCode
)
->String.ofList
// "asdfasdf"
                  `}
                </CodeBlock>
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
                    <animated.svg
                      style={{
                        transform: logoStyles.reTransform,
                        position: 'absolute',
                      }}
                      xmlns="http://www.w3.org/2000/svg"
                      height={logoSize * 2}
                      width={logoSize * 2}
                      viewBox="0 0 578 218"
                    >
                      <path
                        d="M128.128 197h-24.426l-12.036-22.892H75.618V197H54.024v-82.718h37.17c21.948 0 34.456 10.62 34.456 29.028 0 12.508-5.192 21.712-14.75 26.668L128.128 197zm-52.51-65.49v25.37h15.694c8.732 0 13.806-4.484 13.806-12.862 0-8.142-5.074-12.508-13.806-12.508H75.618zm63.838-17.228h65.254v17.228h-43.66v15.458h39.412v17.11l-39.412.118v15.576h44.84V197h-66.434v-82.718z"
                        fill="#FFF"
                        id="re"
                      />
                    </animated.svg>
                    <animated.svg
                      style={{
                        transform: logoStyles.camlTransform,
                        position: 'absolute',
                      }}
                      xmlns="http://www.w3.org/2000/svg"
                      height={logoSize * 3}
                      width={logoSize * 3}
                      viewBox="0 0 524.82 144.28"
                    >
                      <path
                        d="M86.08 127c-.2-1.42.2-2.84-.23-4.18-.36-1.16-1.2-1.27-1.76-2.22-1.46-2.48-2.96-5.7-3.1-8.75-.13-2.74-1.13-5.2-1.27-7.92-.07-1.3.09-2.66.04-3.95-.03-.63-.06-1.18-.19-1.86-.03-.17-.14-.87-.2-1.15l.35-.84c-.15-.3 2.9-.2 3.81-.2 1.55.03 3 .1 4.54.18 3.15.16 6.02.12 9.08-.35 6.83-1.06 9.97-3.85 11.58-5 6.27-4.53 9.15-11.93 9.15-11.93 1.03-2.3 1.03-6.43 3.25-8.27 2.61-2.18 7-2.02 10-3.36 1.76-.78 3.03-1.2 4.82-.83 1.34.27 3.73 1.82 4.29-.35-.45-.29-.62-.81-.86-1.1 2.47-.25.05-5.99-.93-7.14-1.52-1.77-4.04-2.58-6.72-3.29-3.2-.84-6.08-1.82-9.08-1.23-5.25 1.03-4.85-1.97-7.94-1.97-3.71 0-10.3.18-11.45 3.78-.53 1.69-1.07 1.76-2 3.05-.78 1.1.14 2.08-.25 3.34-.41 1.3-1.01 5.87-1.63 7.46-1.06 2.7-2.32 6.06-4.65 6.06-3.26.4-5.82.52-8.47-.44-1.59-.58-4.26-1.48-5.58-2.04-6.08-2.56-7.08-5.37-7.08-5.37-.66-1.08-2.38-2.82-3.02-5.09-.71-2.5-1.9-4.59-2.39-5.9-.5-1.34-1.7-3.5-2.64-5.84-1.2-2.99-2.9-5.22-4.14-6.33-1.9-1.69-3.65-4.3-7.5-3.54-.68.13-3.18.25-5.1 1.85-1.3 1.1-1.7 3.34-2.91 5.24-.7 1.1-1.92 4.24-3.04 6.86-.78 1.82-1.14 3.18-1.98 3.85-.66.53-1.47 1.2-2.45.83a7.49 7.49 0 01-1.93-1.13c-.89-.7-2.91-4.14-4.15-6.68a57.02 57.02 0 00-4.71-7.29c-1.91-2.56-3.04-3.22-5.86-3.22-6.07 0-6.53 3.4-9.2 8.34-1.17 2.17-1.6 5.62-3.95 8.31-1.35 1.55-5.64 7.9-8.62 8.98v-.03l-.01.03v45.32-.29c.2-.59.4-1.15.64-1.66 1.15-2.46 3.83-4.74 5.32-7.27.8-1.37 1.73-2.72 2.27-4.16.46-1.25.68-3.1 1.35-4.18.82-1.33 2.1-1.78 3.4-1.99 2.06-.34 3.8 2.95 6.44 4.17 1.12.51 6.28 2.34 7.83 2.71 2.55.61 5.38 1.12 7.97 1.65 1.39.28 2.71.44 4.14.58 1.28.13 6.08.3 6.38.64-2.44 1.24-3.87 4.73-4.79 7.2-.95 2.58-1.62 5.45-2.77 7.96-1.28 2.79-3.96 3.95-3.64 7.19.12 1.3.35 2.65.14 4.07-.23 1.5-.84 2.67-1.28 4.14-.56 1.92-1.24 8.1-2.11 9.92l5.34-.67c.59-1.39 1.12-7.24 1.31-7.8 1-2.93 2.33-5.35 4.36-7.61 1.99-2.22 1.89-5.07 3.05-7.64 1.25-2.8 2.94-5.04 4.53-7.67 2.89-4.76 4.79-10.77 10.91-12 .65-.13 4.4 2.58 6.07 4.19 1.9 1.83 3.99 3.95 5.24 6.48 2.42 4.9 4.48 11.98 5.26 15.9.44 2.24.8 2.37 2.32 4.15.7.82 2.1 3.37 2.55 4.34.48 1.05 1.22 3.42 1.8 4.64.34.72 1.24 2.94 1.89 4.85l4.98-.15c.02.04.11-.02.13.02l-.05-.12a55.45 55.45 0 01-4.9-16.28z"
                        fill="#FFF"
                        id="caml"
                      />
                    </animated.svg>
                  </animated.div>
                )}
                description="Usable on bucklescript or native"
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
              <NextPrevious currentUrl={null} nav={nav} />
            </div>
          </main>
        </ContentContainer>
      </AppContainer>
    </ThemeProvider>
  );
};
