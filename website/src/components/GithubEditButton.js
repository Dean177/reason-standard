import React from 'react';
import {css} from 'styled-components';
import { Link } from './Link'
import { GitHub } from './Icon'

export const GithubEditButton = ({ link }) => {
	return (
      <Link
        to={link}
        css={css`
          height: 30px;
          min-height: 30px;
          display: flex;
          align-items: center;
          flex-direction: row;
          text-align: right;
          font-size: 14px;
          font-weight: 500;
          line-height: 1em;
          text-decoration: none;
          color: #555;
          border: 1px solid rgb(211, 220, 228);
          cursor: pointer;
          border-radius: 3px;
          transition: all 0.2s ease-out 0s;
          text-decoration: none;
          color: rgb(36, 42, 49);
          background-color: rgb(255, 255, 255);
          box-shadow: rgba(116, 129, 141, 0.1) 0px 1px 1px 0px;
          height: 30px;
          padding: 5px 16px;

          &:hover {
            background-color: rgb(245, 247, 249);
          }

          svg {
            height: 15px;
            width: 15px;
            display: inline-block;
            margin-right: 5px;
          }
        `}
      >
        <GitHub alt={'Github logo'} />
        Edit on GitHub
      </Link>
  );
}
;
