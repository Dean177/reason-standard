import styled, { css } from 'styled-components';
import React from 'react';
import { Reason, Ocaml } from './Icon'

let keywords = {
  reason: {
    val: 'let',
    typesig: ': ',
    arrow: '=>',
    module: {
      open: '{',
      close: '}',
    },
    moduleSignature: {
      open: '{',
      close: '}',
    },
  },
  ocaml: {
    val: 'val',
    typesig: ' : ',
    arrow: '->',
    module: {
      open: 'struct',
      close: 'end',
    },
    moduleSignature: {
      open: 'sig',
      close: 'end',
    },
  },
};

let SyntaxContext = React.createContext([keywords.reason, () => { }]);

export let SyntaxProvider = ({ children }) => {
  let [syntax, setSyntax] = React.useState('reason');
  return (
    <SyntaxContext.Provider
      children={children}
      value={[
        keywords[syntax],
        () => setSyntax(current => (current === 'reason' ? 'ocaml' : 'reason')),
      ]}
    />
  );
};

export let useSyntax = () => React.useContext(SyntaxContext);


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

  svg {
    width: 1rem;
    height: 2.5rem;
    /* TOOD linear is weird */
    transition: all 0.3s linear;

    // sun icon
    &:first-child {
      transform: ${({ isReason }) =>
        isReason ? 'translateY(0)' : 'translateY(60px)'};
    }

    // moon icon
    &:nth-child(2) {
      transform: ${({ isReason }) =>
        isReason ? 'translateY(-60px)' : 'translateY(0)'};
    }
  }
`;

export const SyntaxToggle = () => {
  let [syntax, toggleSyntax] = useSyntax()
  return (
    <ToggleContainer onClick={toggleSyntax} isReason={syntax === 'reason'}>
      <Reason height={10} width={10} />
      <Ocaml height={10} width={10} />
    </ToggleContainer>
  );
};
