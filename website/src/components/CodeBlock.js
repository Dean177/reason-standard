import * as React from 'react';
import Highlight, { defaultProps } from 'prism-react-renderer';
import prismTheme from 'prism-react-renderer/themes/github';
import refmt from 'reason';
import { useSyntax } from './Syntax';

/** Removes the last token from a code example if it's empty. */
function cleanTokens(tokens) {
  const tokensLength = tokens.length;
  if (tokensLength === 0) {
    return tokens;
  }
  const lastToken = tokens[tokensLength - 1];
  if (lastToken.length === 1 && lastToken[0].empty) {
    return tokens.slice(0, tokensLength - 1);
  }
  return tokens;
}

export const CodeBlock = ({ children, ...props }) => {
  let [syntax, _] = useSyntax();
  let lines = children.split('\n');
  let firstNonEmptyLine = 0;
  while (lines[firstNonEmptyLine].trim() === '') {
    firstNonEmptyLine++;
  }
  let spacesToFirstCharacter = 0;
  while (lines[firstNonEmptyLine][spacesToFirstCharacter] == ' ') {
    spacesToFirstCharacter++;
  }
  let code = lines
    .slice(firstNonEmptyLine, lines.length)
    .map(line => line.slice(spacesToFirstCharacter, line.length))
    .join('\n')
    .toString();
  try {
    if (syntax === 'reason') {
      code = refmt.printRE(refmt.parseML(code));
    }
  } catch (error) {      
    console.error(error.message, code);
  }
  return (
    <Highlight
      {...defaultProps}
      code={code}
      theme={prismTheme}
      language={syntax}
      {...props}
    >
      {({ className, style, tokens, getLineProps, getTokenProps }) => (
        <pre className={className + ' pre'} style={style} p={3}>
          {cleanTokens(tokens).map((line, i) => {
            const lineProps = getLineProps({ line, key: i });
            return (
              <div {...lineProps} key={line + i}>
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
};
