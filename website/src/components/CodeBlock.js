import * as React from 'react';
import Highlight, { defaultProps } from 'prism-react-renderer';
import prismLightTheme from 'prism-react-renderer/themes/nightOwlLight';
import prismDarkTheme from 'prism-react-renderer/themes/nightOwl';
import refmt from 'reason';
import { useSyntax } from './Syntax';
import { useTheme, colors } from '../theme';

// export const CodeBlock = ({ code, ...props }) => {
//   let [syntax, _] = useSyntax();
//   let [theme] = useTheme();
//   let content = React.useMemo(() => {
//     let lines = code.split('\n');
//     let firstNonEmptyLine = 0;
//     while (lines[firstNonEmptyLine].trim() === '') {
//       firstNonEmptyLine++;
//     }
//     let spacesToFirstCharacter = 0;
//     while (lines[firstNonEmptyLine][spacesToFirstCharacter] == ' ') {
//       spacesToFirstCharacter++;
//     }
//     let codeContent = lines
//       .slice(firstNonEmptyLine, lines.length)
//       .map(line => line.slice(spacesToFirstCharacter, line.length))
//       .join('\n')
//       .toString();

//     try {
//       if (!props.language && syntax === 'reason') {
//         codeContent = refmt.printRE(refmt.parseML(code));
//       }
//     } catch (error) {
//       console.error(error.message, code);
//     }
//     return codeContent
//   }, [code]);
//   return (
//     <Highlight
//       {...defaultProps}
//       code={content}
//       theme={theme === 'light' ? prismLightTheme : prismDarkTheme}
//       language={props.language || 'ocaml' || syntax}
//       {...props}
//     >
//       {({ className, style, tokens, getLineProps, getTokenProps }) => (
//         <pre
//           className={'CodeBlock ' + className}
//           style={style}
//           p={3}
//           css={css`
//             border: 1px solid ${colors.grey.base};
//             border-radius: 3px;
//             padding: 10px 12px;
//           `}
//         >
//           {cleanTokens(tokens).map((line, i) => {
//             const lineProps = getLineProps({ line, key: i });
//             return (
//               <div {...lineProps} key={line + i}>
//                 {line.map((token, key) => {
//                   return <span {...getTokenProps({ token, key })} />;
//                 })}
//               </div>
//             );
//           })}
//         </pre>
//       )}
//     </Highlight>
//   );
// };

export const CodeBlock = ({ code, ...props }) => {
  let [syntax, _] = useSyntax();
  let [theme] = useTheme();
  let content = React.useMemo(() => {
    let lines = code.split('\n');
    let firstNonEmptyLine = 0;
    while (lines[firstNonEmptyLine].trim() === '') {
      firstNonEmptyLine++;
    }
    let spacesToFirstCharacter = 0;
    while (lines[firstNonEmptyLine][spacesToFirstCharacter] == ' ') {
      spacesToFirstCharacter++;
    }
    let codeContent = lines
      .slice(firstNonEmptyLine, lines.length)
      .map(line => line.slice(spacesToFirstCharacter, line.length))
      .join('\n')
      .toString();

    // try {
    //   if (!props.language && syntax === 'reason') {
    //     codeContent = refmt.printRE(refmt.parseML(code));
    //   }
    // } catch (error) {
    //   console.error(error.message, code);
    // }
    return codeContent;
  }, [code, syntax]);
  return (
    <pre
      className={'CodeBlock'}
      css={css`
        border: 1px solid ${colors.grey.base};
        border-radius: 3px;
        padding: 10px 12px;
        background-color: ${theme};
      `}
    >
      {content}
    </pre>
  );
};
