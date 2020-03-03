import React from 'react';
import { graphql, navigate, Link } from 'gatsby';
import styled, { css } from 'styled-components';
import _ from 'lodash';
import {
  dimensions,
  colors,
  fonts,
  GlobalStyles,
  ThemeProvider,
  spacing,
} from '../theme';
import {
  AppContainer,
  ContentContainer,
  Main,
  MenuButton,
  MenuButtonContainer,
  NavBar,
  NavBarContainer,  
  PageTitle,
  SidebarContainer,
} from '../components/Layout';
import { CodeBlock } from '../components/CodeBlock';
import { SyntaxProvider, SyntaxToggle } from '../components/Syntax';
import * as lzString from 'lz-string';

let stripCorePrefix = path => path.replace(/Core\./g, '');

function deDupeIncludedModules(moduleElements, modulesByName) {
  let flattenedModuleElements = [];
  moduleElements.forEach(moduleElement => {
    if (moduleElement.tag == 'IncludedModule') {
      let includedModule = modulesByName[moduleElement.value.name];
      includedModule.value.kind.value.forEach(includedModuleElement => {
        flattenedModuleElements.push(includedModuleElement);
      });
    } else {
      flattenedModuleElements.push(moduleElement);
    }
  });
  let seen = {};
  let deDupedModuleElements = [];
  flattenedModuleElements.forEach(moduleElement => {
    if (moduleElement.tag === 'Module') {
      if (seen[moduleElement.value.name]) {
        deDupedModuleElements[seen[moduleElement.value.name]] = moduleElement;
        return;
      } else {
        seen[moduleElement.value.name] = deDupedModuleElements.length;
      }
    }
    deDupedModuleElements.push(moduleElement);
  });
  return deDupedModuleElements;
}

let Json = ({ value }) => (
  <pre
    className="DebugJsonValue"
    css={css`
      width: 100%;
      border: 3px dashed green;
      background-color: black;
      color: green;
      padding: 10px;
      overflow: auto;
    `}
  >
    <code>{JSON.stringify(value, null, 2)}</code>
  </pre>
);

let UnhandledCase = props => {
  if (process.env.NODE_ENV === 'production') {
    throw new Error(`Unhandled case: ${props.case}`);
  }
  return (
    <div
      className="UnhandledCase"
      css={css`
        border: 5px dotted red;
      `}
    >
      <h1>Unhandled Case</h1>
      <Json value={props} />
    </div>
  );
};

const compress = lzString.compressToEncodedURIComponent;

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
  moduleElements,
  modulesByModulePath,
  search,
  collapsed,
  toggleModule,
  path = [],
) {
  return deDupeIncludedModules(moduleElements, modulesByModulePath).map(
    (moduleElement, index) => {
      let pathMatchesSearch = path.some(module => module.includes(search));
      switch (moduleElement.tag) {
        case 'Type':
          let typeLink = linkFor(
            path,
            moduleElement.tag,
            moduleElement.value.name,
          );
          if (
            search.length > 0 &&
            !pathMatchesSearch &&
            !moduleElement.value.name.includes(search)
          ) {
            return null;
          }
          return (
            <div key={typeLink}>
              <Link to={typeLink}>type {moduleElement.value.name}</Link>
            </div>
          );
        case 'Value':
          let valueLink = linkFor(
            path,
            moduleElement.tag,
            moduleElement.value.name,
          );
          if (
            search.length > 0 &&
            !pathMatchesSearch &&
            !moduleElement.value.name.includes(search)
          ) {
            return null;
          }

          return (
            <div key={valueLink}>
              <Link to={valueLink}>{moduleElement.value.name}</Link>
            </div>
          );
        case 'ModuleType':
          let moduleTypeLink = linkFor(
            path,
            moduleElement.tag,
            moduleElement.value.name,
          );
          if (
            search.length > 0 &&
            !pathMatchesSearch &&
            !moduleElement.value.name.includes(search)
          ) {
            return null;
          }
          return (
            <div key={moduleTypeLink}>
              <Link to={moduleTypeLink}>
                module type {moduleElement.value.name}
              </Link>
            </div>
          );
        case 'Module':
          switch (moduleElement.value.kind.tag) {
            case 'ModuleStruct':
              let qualifiedModuleName =
                path.length > 0
                  ? [...path, moduleElement.value.name].join('.')
                  : moduleElement.value.name;

              let isCollapsed = !!collapsed[qualifiedModuleName];
              let moduleLink = linkFor(
                path,
                moduleElement.tag,
                moduleElement.value.name,
              );

              let content = renderSidebarElements(
                moduleElement.value.kind.value,
                modulesByModulePath,
                search,
                collapsed,
                toggleModule,
                [...path, moduleElement.value.name],
              );

              if (
                search.length > 0 &&
                !pathMatchesSearch &&
                !(
                  moduleElement.value.name.includes(search) ||
                  content.filter(
                    e =>
                      Boolean(e) && e.hasOwnProperty('length') && e.length > 0,
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
                      padding-bottom: 8px;
                      padding-top: 6px;

                      .expansion-indicator {
                        margin-right: 8px;
                        cursor: pointer;
                      }
                    }
                    .elements {
                      border-left: 1px solid grey;
                      margin-left: 8px;
                      padding-left: 12px;
                      > * {
                        padding-bottom: 4px;
                        padding-top: 4px;
                      }
                    }
                  `}
                >
                  <div className="name">
                    <div
                      className="expansion-indicator"
                      onClick={() => toggleModule(qualifiedModuleName)}
                    >
                      {isCollapsed ? '▷' : '▽'}
                    </div>
                    <Link to={moduleLink}>module {qualifiedModuleName}</Link>
                  </div>
                  {isCollapsed ? null : (
                    <div className="elements">{content}</div>
                  )}
                </div>
              );
            default:
              return (
                <UnhandledCase
                  key={'DefaultSidebarModule' + index}
                  moduleElement={moduleElement}
                />
              );
          }
        case 'IncludedModule':
          let includedModule = modulesByModulePath[moduleElement.value.name];
          if (includedModule == null) {
            throw new Error(
              `The included module '${moduleElement.value.name}' is missing`,
            );
          }
          if (includedModule.value.kind.tag != 'ModuleStruct') {
            throw new Error(
              `Unmapped case for ${moduleElement.kind.value.name} ${includedModule.value.kind}`,
            );
          }
          return renderSidebarElements(
            includedModule.value.kind.value,
            modulesByModulePath,
            search,
            collapsed,
            toggleModule,
            path,
          );
        case 'Text':
          return null;
        default:
          return (
            <UnhandledCase
              key={'DefaultSidebarModule' + index}
              moduleElement={moduleElement}
            />
          );
      }
    },
  );
}

const Sidebar = ({ moduleElements, moduleByModulePath }) => {
  let [search, setSearch] = React.useState('');
  let [collapsed, setCollapsed] = React.useState({});
  let toggleModule = qualifiedModuleName =>
    setCollapsed(collapsed => ({
      ...collapsed,
      [qualifiedModuleName]: !collapsed[qualifiedModuleName],
    }));
  return (
    <div
      css={css`
        background-color: ${({ theme }) => theme.body};
        display: flex;        
        flex-direction: column;
        height: 100vh;
        width: 100%;
      `}
    >
      <input
        value={search}
        placeholder="Search"
        onChange={e => setSearch(e.target.value)}
        css={css`
          background-color: ${colors.grey.light};
          border-color: ${colors.grey.light};
          border: none;
          border-radius: 8px;
          font-size: 16px;
          margin: 8px;
          padding: 10px;
        `}
      />
      <div
        className="SidebarItems"
        css={css`
          display: flex;
          flex: 1;
          flex-direction: column;
          overflow-y: auto;
          padding-left: 15px;
          padding-top: 15px;
          padding-right: 15px;
          padding-bottom: ${15 + dimensions.navbar}px;
          > * {
            padding: 4px;
          }
        `}
      >
        {renderSidebarElements(
          moduleElements,
          moduleByModulePath,
          search,
          collapsed,
          toggleModule,
        )}
      </div>
    </div>
  );
};

let linkSize = spacing.pageMargin;

const PageAnchor = ({ id, children }) => {
  return (
    <div
      id={id}
      css={css`
        align-items: center;
        display: flex;
        flex-shrink: 0;
        flex-direction: row;
        margin-left: -${linkSize}px;

        .link {
          opacity: 0.3;
          &:hover {
            opacity: 1;
          }
        }
        &:hover {
          .link {
            opacity: 1;
          }
        }

        .link {
          flex-shrink: 0;
          font-size: 15px;
          height: 100%;
          text-align: center;
          width: ${linkSize}px;
          display: flex;
          justify-content: center;

          span {
            width: 17px;
          }
        }
        .content {
          width: calc(100% + ${linkSize}px);
          overflow-x: auto;
        }
      `}
    >
      <a href={`/api#${id}`} className="link">
        <span>🔗</span>
      </a>
      <div className="content">{children}</div>
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
          margin-right: 10px;
        }
        .name {
          color: ${colors.red.base};
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
  moduleType: ({ name }) => (
    <span
      css={css`
        align-items: flex-end;
        display: flex;
        flex-direction: row;

        .keyword {
          font-family: ${fonts.monospace};
          margin-right: 10px;
        }
        .name {
          color: ${colors.red.base};
        }
      `}
    >
      <span className="keyword">
        <h2>module type</h2>
      </span>
      <span className="name">
        <h1>{name}</h1>
      </span>
    </span>
  ),
};

let renderTextElements = (elements = [], parentPath = []) => {
  return elements.map(({ tag, value }, index) => {
    switch (tag) {
      case 'Raw':
        return <span key={index}>{value}</span>;
      case 'Link':
        return (
          <a key={index} href={value.target}>
            {renderTextElements(value.content, parentPath)}
          </a>
        );
      case 'Newline':
        return <pre key={index}>{'\n'}</pre>;
      case 'Emphasize':
        return <em key={index}>{(renderTextElements(value), parentPath)}</em>;
      case 'Bold':
        return <b key={index}>{renderTextElements(value, parentPath)}</b>;
      case 'Code':
        return (
          <code
            key={index}
            css={css`
              background: #fbfafa;
              border: 1px solid #eee;
              font-family: ${fonts.monospace};
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
          <ul
            key={index}
            css={css`
              padding-left: 30px;
              padding-bottom: 10px;
              li {
                padding-bottom: 8px;
              }
            `}
          >
            {value.map((listElement, subIndex) => (
              <li key={subIndex}>
                {renderTextElements(listElement, parentPath)}
              </li>
            ))}
          </ul>
        );
      case 'Ref':
        let content =
          value.reference.content.length === 1 &&
          value.reference.content[0].tag === 'Code'
            ? stripCorePrefix(value.reference.content[0].value)
            : renderTextElements(value.reference.content, parentPath);
        return (
          <a
            key={index}
            href={`/api#${stripCorePrefix(value.reference.target)}`}
          >
            {content}
          </a>
        );
      case 'Title':
        let rawContent = value.content
          .filter(textElement => textElement.tag === 'Raw')
          .map(({ value }) => value)
          .join('_');
        let titleId = idFor(parentPath, 'Title', rawContent);
        return (
          <PageAnchor className="Title" id={titleId} key={index}>
            {React.createElement(`h${value.size + 1}`, {
              key: index,
              id: value.label,
              children: renderTextElements(value.content, parentPath),
            })}
          </PageAnchor>
        );

      case 'CodePre':
        return (
          <div
            key={index}
            className="CodePre"
            css={css`
              font-family: ${fonts.monospace};
              overflow: auto;
              position: relative;
              width: 100%;

              .try {
                border: none;
                bottom: 0;
                cursor: pointer;
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
            <CodeBlock code={value} />
            {/* TODO get the playground working */}
            {/* <button
              className="try"
              onClick={() => {
                navigate(`/try?ocaml=${compress(value)}`);
              }}
            >
              Try
            </button> */}
          </div>
        );
      // case 'Custom':
      //   return (
      //     <pre key={index} clas>
      //       <code>{JSON.stringify({ tag, value }, null, 2)}</code>
      //     </pre>
      //   );
      case 'Enum':
        return (
          <ul
            key={index}
            css={css`
              padding-left: 30px;
              padding-bottom: 10px;
              li {
                padding-bottom: 8px;
              }
            `}
          >
            {value.map((enumItem, enumIndex) => (
              <li key={enumIndex}>
                {renderTextElements(enumItem, parentPath)}
              </li>
            ))}
          </ul>
        );
      default:
        return (
          <UnhandledCase
            key={'DefaultTextElement' + index}
            element={{ tag, value }}
          />
        );
    }
  });
};

let TextElement = ({ elements, path }) => {
  return (
    <div
      css={css`
        padding-top: 12px;
        padding-bottom: 12px;
      `}
    >
      {renderTextElements(elements, path)}
    </div>
  );
};

let TypeSignature = ({ signature }) => {
  return (
    <pre>
      <code children={stripCorePrefix(signature.rendered)} />
    </pre>
  );
};

let ValueContainer = props => (
  <div
    className="ValueContainer"
    css={css`
      margin-bottom: 25px;
      margin-top: 15px;
    `}
    {...props}
  />
);

let ValueWrapper = styled.div`
  align-items: flex-start;
  display: flex;
  background-color: ${colors.blue.lightest};
  border-radius: 3px;
  border-left: 4px solid ${colors.blue.base};
  flex: 1;
  flex-direction: row;
  padding-top: 10px;
  padding-bottom: 10px;
  padding-left: 15px;
  padding-right: 15px;
  width: 100%;
  overflow-x: auto;
`;

let Value = ({ path, name, type, info, parameters, ...value }) => {
  return (
    <ValueContainer>
      <PageAnchor id={idFor(path, 'Value', name)}>
        <ValueWrapper>
          {/* // TODO syntax highlighting */}
          <pre>
            <code>let {name}: </code>
          </pre>
          <TypeSignature signature={type} />
        </ValueWrapper>
      </PageAnchor>
      {info && (
        <div
          css={css`
            padding-top: 10px;
            padding-bottom: 10px;
          `}
        >
          <TextElement
            elements={info.description.value}
            path={[...path, name]}
          />
        </div>
      )}
    </ValueContainer>
  );
};

let ModuleCard = props => (
  <div
    className="ModuleCard"
    css={css`
      margin-bottom: 3rem;
    `}
    {...props}
  />
);

function renderModuleElements(moduleElements, modulesByName, path = []) {
  return deDupeIncludedModules(moduleElements, modulesByName).map(
    (moduleElement, index) => {
      switch (moduleElement.tag) {
        case 'Text':
          return (
            <div
              key={index}
              css={css`
                padding: 10px 0px;
              `}
            >
              <TextElement elements={moduleElement.value} path={path} />
            </div>
          );
        case 'Type':
          return (
            <ValueContainer key={index}>
              <PageAnchor
                id={idFor(path, moduleElement.tag, moduleElement.value.name)}
              >
                <ValueWrapper>
                  <pre>
                    <code>
                      type {moduleElement.value.name}
                      {moduleElement.value.parameters.length > 0 &&
                        `(${moduleElement.value.parameters})`}
                      {moduleElement.value.manifest ? ': ' : ''}
                    </code>
                  </pre>
                  {moduleElement.value.manifest && (
                    <TypeSignature
                      signature={moduleElement.value.manifest.value}
                    />
                  )}
                </ValueWrapper>
              </PageAnchor>
              {moduleElement.value.info && (
                <div
                  css={css`
                    padding-left: 15px;
                  `}
                >
                  <TextElement
                    elements={moduleElement.value.info.description.value}
                  />
                </div>
              )}
            </ValueContainer>
          );
        case 'Value':
          return <Value key={index} path={path} {...moduleElement.value} />;
        case 'ModuleType':
          let moduleTypeId = idFor(
            path,
            moduleElement.tag,
            moduleElement.value.name,
          );
          return (
            <ModuleCard key={moduleTypeId + index}>
              <PageAnchor id={moduleTypeId}>
                <Identifiers.moduleType name={moduleTypeId} />
              </PageAnchor>
              {moduleElement.value.elements &&
                renderModuleElements(
                  moduleElement.value.elements,
                  modulesByName,
                  [...path, moduleElement.value.name],
                )}
            </ModuleCard>
          );
        case 'Module':
          switch (moduleElement.value.kind.tag) {
            case 'ModuleStruct':
              let moduleStructId = idFor(
                path,
                moduleElement.tag,
                moduleElement.value.name,
              );
              return (
                <ModuleCard key={moduleStructId + index}>
                  <PageAnchor id={moduleStructId}>
                    <Identifiers.module name={moduleStructId} />
                  </PageAnchor>
                  {renderModuleElements(
                    moduleElement.value.kind.value,
                    modulesByName,
                    [...path, moduleElement.value.name],
                  )}
                </ModuleCard>
              );
            // case 'ModuleAlias':
            //   if (index % 2 == 1) {
            //     return null;
            //   }
            //   let module = modulesByName[moduleElement.value.kind.value.name];
            //   if (module == null) {
            //     throw new Error(
            //       `The module '${moduleElement.value.name} (aliased to ${moduleElement.value.name}) is missing`,
            //     );
            //   }
            //   if (module.value.kind.tag != 'ModuleStruct') {
            //     throw new Error(
            //       'Unmapped case for ' + kind.value.name + module.value.kind,
            //     );
            //   }
            //   let id = idFor(path, 'ModuleStruct', module.value.name);
            //   return (
            //     <ModuleCard key={id + index}>
            //       <PageAnchor id={id}>
            //         <Identifiers.module
            //           name={'Alias ' + moduleElement.value.kind.value.name}
            //         />
            //       </PageAnchor>
            //       {renderModuleElements(module.value.kind.value, modulesByName)}
            //     </ModuleCard>
            //   );
            default:
              return (
                <UnhandledCase
                  key={'DefaultModule' + index}
                  el={moduleElement}
                />
              );
          }
        // case 'IncludedModule':
        //   let includedModule = modulesByName[moduleElement.value.name];
        //   if (includedModule == null) {
        //     throw new Error(
        //       `The included module '${moduleElement.value.name}' is missing`,
        //     );
        //   }
        //   if (includedModule.value.kind.tag != 'ModuleStruct') {
        //     throw new Error(
        //       `Unmapped case for ${moduleElement.kind.value.name} ${includedModule.value.kind}`,
        //     );
        //   }
        //   return renderModuleElements(
        //     includedModule.value.kind.value,
        //     modulesByName,
        //     path,
        //   );
        default:
          return <UnhandledCase key={index} el={moduleElement} />;
      }
    },
  );
}

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

let moduleIndex = (moduleElements, parentPath = []) => {
  return moduleElements
    .filter(
      element =>
        element != null &&
        element.tag === 'Module' &&
        element.value.kind.tag === 'ModuleStruct',
    )
    .flatMap(modu => {
      let path = [...parentPath, modu.value.name];
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
            <NavBarContainer>
              <NavBar />
            </NavBarContainer>
            <SidebarContainer isOpen={isOpen}>
              <Sidebar
                moduleElements={model.entry_point.value.kind.value}
                moduleByModulePath={moduleByModulePath}
              />
            </SidebarContainer>
            <Main>
              <div
                css={css`
                  display: flex;
                  flex-direction: row;
                  justify-content: space-between;
                  width: 100%;
                `}
              >
                <PageTitle>API</PageTitle>
                <div>
                  <SyntaxToggle />
                </div>
              </div>
              {renderModuleElements(
                model.entry_point.value.kind.value,
                moduleByModulePath,
              )}
            </Main>
          </ContentContainer>
          <MenuButtonContainer>
            <MenuButton
              onClick={() => setIsOpen(open => !open)}
              isOpen={isOpen}
            />
          </MenuButtonContainer>
        </AppContainer>
      </SyntaxProvider>
    </ThemeProvider>
  );
};
