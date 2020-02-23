import styled, { css } from 'styled-components';
import React from 'react';
import { Reason, Ocaml } from './Icon';
import { colors } from '../theme';

export let keywords = {
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

let SyntaxContext = React.createContext([keywords.reason, () => {}]);

export let SyntaxProvider = ({ children }) => {
  let [syntax, setSyntax] = React.useState('reason');
  let toggle = () =>
    setSyntax(current => (current === 'reason' ? 'ocaml' : 'reason'));
  return (
    <SyntaxContext.Provider children={children} value={[syntax, toggle]} />
  );
};

export let useSyntax = () => React.useContext(SyntaxContext);

let logoSize = 40;
const ToggleContainer = styled.button`
  background: ${({ isReason }) =>
    isReason
      ? `linear-gradient(${colors.red.base}, ${colors.red.base})`
      : 'linear-gradient(#f29100, #ec670f)'};
  border: 2px solid ${({ theme }) => theme.toggle.border};
  border-radius: 30px;
  cursor: pointer;
  margin: 0 auto;
  overflow: hidden;
  padding: 0.4rem;
  position: relative;
  width: ${logoSize * 2}px;
  height: ${logoSize}px;

  svg {
    position: absolute;
    top: 0;
    bottom: 0;
    flex-shrink: 0;
    fill: white;
    /* TOOD linear is weird */
    transition: all 0.3s linear;

    // reason logo
    &:first-child {
      transform: ${({ isReason }) =>
        isReason
          ? `translate3d(${-logoSize / 4}px, ${-logoSize / 1.025}px,0)`
          : `translate3d(${-logoSize * 2}px, ${-logoSize / 1.025}px, 0)`};
    }

    // ocaml logo
    &:nth-child(2) {
      transform: ${({ isReason }) =>
        isReason
          ? `translate3d(${logoSize * 2}px, ${-logoSize / 0.9}px, 0)`
          : `translate3d(${-logoSize / 1.1}px, ${-logoSize / 0.9}px,0)`};
    }
  }
`;

export const SyntaxToggle = () => {
  let [syntax, toggleSyntax] = useSyntax();
  return (
    <ToggleContainer onClick={toggleSyntax} isReason={syntax === 'reason'}>
      <Reason height={logoSize * 2.5} width={logoSize * 2.5} />
      <Ocaml height={logoSize * 3.25} width={logoSize * 3.25} />
    </ToggleContainer>
  );
};
