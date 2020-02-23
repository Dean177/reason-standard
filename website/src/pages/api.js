import React from 'react';
import Helmet from 'react-helmet';
import { graphql, navigate, Link } from 'gatsby';
import styled, { css } from 'styled-components';
import _ from 'lodash';
import {
  breakpoints,
  colors,
  fonts,
  GlobalStyles,
  ThemeProvider,
} from '../theme';
import {
  MenuButton,
  NavBar,
  ContentContainer,
  AppContainer,
  PageTitle,
} from '../components/Layout';
import { SyntaxProvider, SyntaxToggle } from '../components/Syntax';
import * as lzString from 'lz-string';
import refmt from 'reason';
const compress = lzString.compressToEncodedURIComponent;
const decompress = lzString.decompressFromEncodedURIComponent;

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

let idFor = (path, tag, name) => {
  // The names of values and types may not be unique within a module
  // (for example float.radians refers to a function and a type)
  // We need to be able to distinguish between them.
  let prefix = tag === 'Type' ? 'type_' : '';
  return `${path.join('.')}${path.length > 0 ? '.' : ''}${prefix}${name}`;
};

let linkFor = (path, tag, name) => {
  return `/api#${idFor(path, tag, name)}`;
};

function renderSidebarElements(
  module_elements,
  modulesByName,
  search = '',
  path = [],
) {
  return module_elements.map((module_element, index) => {
    switch (module_element.tag) {
      case 'Type':
        let typeLink = linkFor(
          path,
          module_element.tag,
          module_element.value.name,
        );
        if (search.length > 0 && !module_element.value.name.includes(search)) {
          return null;
        }
        return (
          <div key={typeLink}>
            <Link to={typeLink}>type {module_element.value.name}</Link>
          </div>
        );
      case 'Value':
        let valueLink = linkFor(
          path,
          module_element.tag,
          module_element.value.name,
        );
        if (search.length > 0 && !module_element.value.name.includes(search)) {
          return null;
        }

        return (
          <div key={valueLink}>
            <Link to={valueLink}>{module_element.value.name}</Link>
          </div>
        );
      case 'ModuleType':
        let moduleTypeLink = linkFor(
          path,
          module_element.tag,
          module_element.value.name,
        );
        if (search.length > 0 && !module_element.value.name.includes(search)) {
          return null;
        }
        return (
          <div key={moduleTypeLink}>
            <Link to={moduleTypeLink}>
              module type {module_element.value.name}
            </Link>
          </div>
        );
      case 'Module':
        switch (module_element.value.kind.tag) {
          case 'ModuleStruct':
            let moduleLink = linkFor(
              path,
              module_element.tag,
              module_element.value.name,
            );
            let content = renderSidebarElements(
              module_element.value.kind.value,
              modulesByName,
              search,
              [...path, module_element.value.name],
            );

            if (
              search.length > 0 &&
              !(
                module_element.value.name.includes(search) ||
                content.filter(
                  e => Boolean(e) && e.hasOwnProperty('length') && e.length > 0,
                ).length > 0
              )
            ) {
              return null;
            }
            return (
              <div
                key={moduleLink}
                css={css`
                  margin-top: -5px;
                  .name {
                    display: flex;
                    flex-direction: row;
                    font-weight: bold;
                    font-size: 1.2em;
                    padding-bottom: 12px;
                    padding-top: 10px;

                    .expansion-indicator {
                      margin-right: 8px;
                    }
                  }
                  .elements {
                    border-left: 1px solid grey;
                    padding-left: 12px;
                  }
                `}
              >
                <div className="name">
                  <div className="expansion-indicator">V</div>
                  <Link to={moduleLink}>
                    module{' '}
                    {path.length > 0
                      ? [...path, module_element.value.name].join('.')
                      : module_element.value.name}
                  </Link>
                </div>
                <div className="elements">{content}</div>
              </div>
            );
          default:
            return (
              <pre
                key={index}
                css={css`
                  border: 6px dashed blue;
                `}
              >
                {JSON.stringify(module_element, null, 2)}
              </pre>
            );
        }
      case 'IncludedModule':
        let includedModule = modulesByName[module_element.value.name];
        if (includedModule == null) {
          throw new Error(
            `The included module '${module_element.value.name}' is missing`,
          );
        }
        if (includedModule.value.kind.tag != 'ModuleStruct') {
          throw new Error(
            `Unmapped case for ${module_element.kind.value.name} ${includedModule.value.kind}`,
          );
        }
        return renderSidebarElements(
          includedModule.value.kind.value,
          modulesByName,
          search,
          path,
        );
      case 'Text':
        return null;
      default:
        return (
          <pre
            key={index}
            css={css`
              border: 5px dotted darkgoldenrod;
            `}
          >
            {JSON.stringify(module_element, null, 2)}
          </pre>
        );
    }
  });
}

const Sidebar = ({ moduleElements, moduleByModulePath }) => {
  let [search, setSearch] = React.useState('');
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
          flex: 1;
          height: 100vh;
          display: flex;
          flex-direction: column;
          overflow: auto;
          padding-left: 15px;
          padding-top: 15px;
          > * {
            padding: 5px;
          }
        `}
      >
        {renderSidebarElements(moduleElements, moduleByModulePath, search)}
      </div>
    </div>
  );
};

let linkSize = 30;
let linkPadding = 5;

const PageAnchor = ({ link, children }) => {
  return (
    <div
      id={link}
      css={css`
        align-items: center;
        display: flex;
        flex-shrink: 0;
        flex-direction: row;
        margin-left: -${linkSize}px;
        position: relative;
        a {
          opacity: 0.1;
        }
        &:hover {
          a {
            opacity: 1;
          }
        }
      `}
    >
      <a
        href={`${location.pathname}#${link}`}
        css={css`
          font-size: 13px;
          height: 100%;
          text-align: center;
          width: ${linkSize}px;
          padding: ${linkPadding}px;
        `}
      >
        🔗
      </a>
      {children}
    </div>
  );
};

let Identifiers = {
  module: ({ name }) => (
    <span
      css={css`
        align-items: flex-end;
        display: flex;
        flex-direction: row;

        .keyword {
          font-family: ${fonts.monospace};
          letter-spacing: 1.1px;
          margin-right: 10px;
        }
        .name {
        }
      `}
    >
      <span className="keyword">
        <h2>module</h2>
      </span>
      <span className="name">
        <h1>{name}</h1>
      </span>
    </span>
  ),
};

let renderElements = (elements = [], parentPath = []) => {
  // TODO take the parent path to link to section headings
  return elements.map(({ tag, value }, index) => {
    switch (tag) {
      case 'Raw':
        return <span key={index}>{value}</span>;
      case 'Link':
        return (
          <a key={index} href={value.target}>
            {renderElements(value.content)}
          </a>
        );
      case 'Newline':
        return <pre key={index}>{'\n'}</pre>;
      case 'Emphasize':
        return <em key={index}>{renderElements(value)}</em>;
      case 'Bold':
        return <b key={index}>{renderElements(value)}</b>;
      case 'Code':
        return (
          <code
            key={index}
            css={css`
              background: #fbfafa;
              border: 1px solid #eee;
              font-family: 'Lucida Console', Monaco, monospace;
              padding: 2px;
              border-radius: 1px;
              overflow: auto;
            `}
          >
            {value}
          </code>
        );

      case 'List':
        return (
          <ul key={index}>
            {value.map((listElement, subIndex) => (
              <li key={subIndex}>{renderElements(listElement)}</li>
            ))}
          </ul>
        );
      case 'Ref':
        return (
          <a key={index} href={`/doc#${value.reference.target}`}>
            {renderElements(value.reference.content)}
          </a>
        );
      case 'Title':
        return (
          <PageAnchor link={value.name}>
            {React.createElement(`h${value.size}`, {
              key: index,
              id: value.label,
              children: renderElements(value.content),
            })}
          </PageAnchor>
        );

      case 'Custom':
        return (
          <pre key={index}>
            <code>{JSON.stringify({ tag, value }, null, 2)}</code>
          </pre>
        );
      case 'CodePre':
        // TODO syntax higlighting
        // TODO make it live
        return (
          <pre
            key={index}
            css={css`
              font-family: 'Lucida Console', Monaco, monospace;
              background-color: lightyellow;
              border-radius: 3px;
              padding: 5px 8px;
              border: 1px solid lightgoldenrodyellow;
              width: 100%;
              overflow: auto;
              position: relative;
              width: 100%;

              .try {
                border: none;
                bottom: 0;
                font-size: 14px;
                padding: 4px;
                position: absolute;
                right: 0;
                opacity: 0.8;

                &:hover {
                  opacity: 1;
                }
              }
            `}
          >
            <code>{value}</code>
            <button
              className="try"
              onClick={() => {
                navigate(`/try?ocaml=${compress(value)}`);
              }}
            >
              Try
            </button>
          </pre>
        );

      default:
        return (
          <pre
            key={index}
            css={css`
              border: 5px dashed red;
            `}
          >
            {JSON.stringify({ tag, value }, null, 2)}
          </pre>
        );
    }
  });
};

let TextElement = ({ elements }) => {
  return (
    <div
      css={css`
        padding: 12px;
      `}
    >
      {renderElements(elements)}
    </div>
  );
};

let TypeSignature = ({ signature }) => {
  return (
    <pre
      css={css`
        border: 2px solid orange;
      `}
    >
      <code dangerouslySetInnerHTML={{ __html: signature.rendered }} />
      {/* TODO */}
      {/* <code>{JSON.stringify(signature.raw, null, 2)}</code> */}
    </pre>
  );
};

let ValueOrTypeContainer = props => (
  <div
    css={css`
      border: 1px solid lightseagreen;
      margin-bottom: 25px;
      margin-top: 15px;
      width: 100%;
      overflow: auto;
    `}
    {...props}
  />
);

let Value = ({ path, name, type, info, parameters, ...value }) => {
  // TODO syntax highlighting
  return (
    <ValueOrTypeContainer>
      <PageAnchor link={idFor(path, 'Value', name)}>
        <div
          css={css`
            align-items: center;
            display: flex;
            background-color: white;
            border-left: 5px solid lightblue;
            flex: 1;
            flex-direction: row;
            padding: 10px 15px;
          `}
        >
          <pre>
            <code>let {name}: </code>
          </pre>
          <TypeSignature signature={type} />
        </div>
      </PageAnchor>
      {info && (
        <div
          css={css`
            border-left: 5px solid lightgreen;
          `}
        >
          <TextElement elements={info.description.value} />
        </div>
      )}
      {parameters.value.length > 0 && (
        <pre
          css={css`
            border: 5px dashed red;
          `}
        >
          {'params'}
          {JSON.stringify(parameters, null, 2)}
        </pre>
      )}
    </ValueOrTypeContainer>
  );
};

let renderModuleElements = (module_elements, modulesByName, path = []) => {
  return module_elements.map((module_element, index) => {
    switch (module_element.tag) {
      case 'Text':
        return <TextElement key={index} elements={module_element.value} />;
      case 'Type':
        // TODO syntax highlighting
        return (
          <ValueOrTypeContainer>
            <PageAnchor
              link={idFor(
                path,
                module_element.tag,
                module_element.value.name,
              )}
            >
              <div
                css={css`
                  align-items: center;
                  display: flex;
                  background-color: white;
                  border-left: 5px solid lightblue;
                  flex: 1;
                  flex-direction: row;
                  padding: 10px 15px;
                `}
              >
                <pre>
                  <code>
                    type {module_element.value.name}
                    {module_element.value.parameters.length > 0 &&
                      `(${module_element.value.parameters})`}
                    {module_element.value.manifest ? ': ' : ''}
                  </code>
                </pre>
                {module_element.value.manifest && (
                  <TypeSignature
                    signature={module_element.value.manifest.value}
                  />
                )}
              </div>
            </PageAnchor>
            {module_element.value.info && (
              <TextElement
                elements={module_element.value.info.description.value}
              />
            )}
            {/* <pre
              key={index}
              css={css`
                border: 3px solid lightyellow;
              `}
            >
              {JSON.stringify(module_element, null, 2)}
            </pre> */}
          </ValueOrTypeContainer>
        );
      case 'Value':
        return <Value key={index} path={path} {...module_element.value} />;
      case 'Module':
        switch (module_element.value.kind.tag) {
          case 'ModuleStruct':
            return (
              <div
                key={index}
                css={css`
                  background-color: white;
                  margin-bottom: 3rem;
                  padding: 10px;
                `}
              >
                <PageAnchor
                  link={idFor(
                    path,
                    module_element.tag,
                    module_element.value.name,
                  )}
                >
                  <Identifiers.module name={module_element.value.name} />
                </PageAnchor>
                {renderModuleElements(
                  module_element.value.kind.value,
                  modulesByName,
                  [...path, module_element.value.name],
                )}
                <hr />
              </div>
            );
          case 'ModuleAlias':
            let module = modulesByName[module_element.value.kind.value.name];
            if (module == null) {
              throw new Error(
                `The module '${module_element.value.name} (aliased to ${module_element.value.name}) is missing`,
              );
            }
            if (module.value.kind.tag != 'ModuleStruct') {
              throw new Error(
                'Unmapped case for ' + kind.value.name + module.value.kind,
              );
            }
            return (
              <div
                key={index}
                css={css`
                  background-color: white;
                  margin-bottom: 3rem;
                  padding: 10px;
                `}
              >
                <PageAnchor link={idFor(path, 'ModuleStruct', module.value.name)}>
                  <Identifiers.module
                    name={module_element.value.kind.value.name}
                  />
                </PageAnchor>
                {renderModuleElements(module.value.kind.value, modulesByName)}
                <hr />
              </div>
            );
          default:
            return (
              <pre
                key={index}
                css={css`
                  border: 6px dashed blue;
                `}
              >
                {JSON.stringify(module_element, null, 2)}
              </pre>
            );
        }
      case 'IncludedModule':
        let includedModule = modulesByName[module_element.value.name];
        if (includedModule == null) {
          throw new Error(
            `The included module '${module_element.value.name}' is missing`,
          );
        }
        if (includedModule.value.kind.tag != 'ModuleStruct') {
          throw new Error(
            `Unmapped case for ${module_element.kind.value.name} ${includedModule.value.kind}`,
          );
        }
        return renderModuleElements(
          includedModule.value.kind.value,
          modulesByName,
        );
      default:
        return (
          <pre
            key={index}
            css={css`
              border: 5px dotted darkgoldenrod;
            `}
          >
            {JSON.stringify(module_element, null, 2)}
          </pre>
        );
    }
  });
};

export const pageQuery = graphql`
  query {
    site {
      siteMetadata {
        docsLocation
      }
    }
    odocModel {
      internal {
        content
      }
    }
  }
`;

let moduleIndex = (module_elements, parent_path = []) => {
  return module_elements
    .filter(
      element =>
        element != null &&
        element.tag === 'Module' &&
        element.value.kind.tag === 'ModuleStruct',
    )
    .flatMap(modu => {
      let path = [...parent_path, modu.value.name];
      return [[path, modu], ...moduleIndex(modu.value.kind.value, path)];
    });
};

export default ({ data }) => {
  let [isOpen, setIsOpen] = React.useState(false);
  const { odocModel } = data;

  let model = JSON.parse(odocModel.internal.content);
  let moduleByModulePath = _.fromPairs(
    _.map(moduleIndex(_.values(model.modules)), ([path, module]) => [
      path.join('.'),
      module,
    ]),
  );

  return (
    <ThemeProvider>
      <SyntaxProvider>
        <GlobalStyles />
        <AppContainer>
          <ContentContainer>
            <NavBar />
            <div
              css={css`
                background-color: blue;
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
              <Sidebar
                moduleElements={model.entry_point.value.kind.value}
                moduleByModulePath={moduleByModulePath}
              />
            </div>
            <div
              css={css`
                display: flex;
                flex: 1;
                flex-direction: row;
                width: 100%;
                height: 100%;
              `}
            >
              <div
                css={css`
                  display: none;
                  position: static;
                  @media (min-width: ${breakpoints.desktop}px) {
                    display: flex;
                  }
                `}
              >
                <Sidebar
                  moduleElements={model.entry_point.value.kind.value}
                  moduleByModulePath={moduleByModulePath}
                />
              </div>
              <main
                css={css`
                  overflow: auto;
                  display: flex;
                  flex: 1;
                  flex-direction: column;
                  height: 100%;
                  width: 100%;
                  padding: 0px 30px;
                  padding-top: 3rem;
                  padding-bottom: 3rem;

                  @media (min-width: ${breakpoints.desktop}px) {
                    max-width: 750px;
                  }
                `}
              >
                <PageTitle>API</PageTitle>
                {renderModuleElements(
                  model.entry_point.value.kind.value,
                  moduleByModulePath,
                )}
                {/* <pre>{JSON.stringify(_.mapValues(moduleByModulePath, _ => []), null, 2)}</pre> */}
              </main>
            </div>
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
      </SyntaxProvider>
    </ThemeProvider>
  );
};
