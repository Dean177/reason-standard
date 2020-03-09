import * as React from 'react';
import Highlight, { defaultProps } from 'prism-react-renderer';
import prismLightTheme from 'prism-react-renderer/themes/nightOwlLight';
import prismDarkTheme from 'prism-react-renderer/themes/nightOwl';
import { useSyntax } from './Syntax';
import { useTheme, colors } from '../theme';

export const CodeBlock = ({ code, ...props }) => {
  let [syntax, _] = useSyntax();
  let [theme] = useTheme();
  let language = typeof code === 'object' ? syntax : props.language || 'ocaml';
  let lines = (typeof code === 'object' ? code[syntax] : code).split('\n');
  let firstNonEmptyLine = 0;
  while (lines[firstNonEmptyLine].trim() === '') {
    firstNonEmptyLine++;
  }
  let spacesToFirstCharacter = 0;
  while (lines[firstNonEmptyLine][spacesToFirstCharacter] == ' ') {
    spacesToFirstCharacter++;
  }
  let content = lines
    .slice(firstNonEmptyLine, lines.length)
    .map(line => line.slice(spacesToFirstCharacter, line.length))    
    .join('\n')
    .toString();

  return (
    <Highlight
      {...defaultProps}
      code={content}
      theme={theme === 'light' ? prismLightTheme : prismDarkTheme}
      language={props.language || 'ocaml' || syntax}
      {...props}
    >
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <pre
          className={'CodeBlock ' + className}
          style={style}
          p={3}
          css={css`
            border: 1px solid ${({ theme }) => theme.card.background};
            border-radius: 3px;
            font-size: 14px;
            padding: 10px 12px;
            width: 100%;
            overflow-x: auto;
          `}
        >
          {tokens.map((line, i) => {
            return (
              <div {...getLineProps({ line, key: i })}>
                {line.map((token, key) => {
                  return <span {...getTokenProps({ token, key })} />;
                })}
              </div>
            );
          })}
        </pre>
      )}
    </Highlight>
  );

  // return (
  //   <pre
  //     className={'CodeBlock'}
  //     css={css`
  //       border: 1px solid ${colors.grey.base};
  //       border-radius: 3px;
  //       padding: 10px 12px;
  //       background-color: ${theme};
  //     `}
  //   >
  //     {content}
  //   </pre>
  // );
};
