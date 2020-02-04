import React from 'react';
import {Link} from "./Link";

export const NextPrevious = (props) => {
  const { nav, mdx } = props;
  const currentIndex = nav.findIndex(({url}) => url === mdx.fields.slug);
  let next;
  let previous;
  
  if (currentIndex === undefined) {
    // We are on the index page
    next = nav[0];
    previous = null;
  } else if (currentIndex === 0) {
    next = nav[currentIndex+1]
    previous = null;
  } else if (currentIndex === (nav.length-1)) {
    next = null;
    previous = nav[currentIndex-1]
  } else if (currentIndex) {
    next = nav[currentIndex+1];
    previous = nav[currentIndex-1];
  }
  console.log({ currentIndex, next, previous }, nav, mdx);
  return (
    <div
      css={css`
        margin: 0px;
        padding: 0px;
        width: auto;
        display: grid;
        grid-template-rows: auto;
        column-gap: 24px;
        grid-template-columns: calc(50% - 8px) calc(50% - 8px);

        .previousBtn {
          cursor: pointer;
          margin: 0px;
          padding: 0px;
          position: relative;
          display: flex;
          flex-direction: row;
          align-items: center;
          place-self: stretch;
          color: rgb(36, 42, 49);
          background-color: rgb(255, 255, 255);
          border-radius: 3px;
          border: 1px solid rgb(230, 236, 241);
          transition: border 200ms ease 0s;
          box-shadow: rgba(116, 129, 141, 0.1) 0px 3px 8px 0px;
          text-decoration: none;
        }

        .leftArrow {
          display: block;
          margin: 0px;
          color: rgb(157, 170, 182);
          flex: 0 0 auto;
          font-size: 24px;
          transition: color 200ms ease 0s;
          padding: 16px;
          padding-right: 16px;
        }

        .preRightWrapper {
          display: block;
          margin: 0px;
          flex: 1 1 0%;
          padding: 16px;
          text-align: right;
        }

        .smallContent {
          display: block;
          margin: 0px;
          padding: 0px;
          color: #6e6e6e;
        }

        .smallContent span {
          font-size: 12px;
          line-height: 1.625;
          font-weight: 400;
        }

        .nextPreviousTitle {
          display: block;
          margin: 0px;
          padding: 0px;
          transition: color 200ms ease 0s;
        }

        .nextPreviousTitle span {
          font-size: 16px;
          line-height: 1.5;
          font-weight: 500;
        }

        .nextBtn {
          cursor: pointer;
          -moz-box-align: center;
          -moz-box-direction: normal;
          -moz-box-orient: horizontal;
          margin: 0px;
          padding: 0px;
          position: relative;
          display: flex;
          flex-direction: row;
          align-items: center;
          place-self: stretch;
          color: rgb(36, 42, 49);
          background-color: rgb(255, 255, 255);
          border-radius: 3px;
          border: 1px solid rgb(230, 236, 241);
          transition: border 200ms ease 0s;
          box-shadow: rgba(116, 129, 141, 0.1) 0px 3px 8px 0px;
          text-decoration: none;
        }

        .rightArrow {
          flex: 0 0 auto;
          font-size: 24px;
          transition: color 200ms ease 0s;
          padding: 16px;
          padding-left: 16px;
          display: block;
          margin: 0px;
          color: rgb(157, 170, 182);
        }

        .nextRightWrapper {
          display: block;
          margin: 0px;
          padding: 16px;
          flex: 1 1 0%;
        }

        .nextBtn:hover,
        .previousBtn:hover {
          color: rgb(116, 76, 188);
          text-decoration: none;
          border: 1px solid rgb(116, 76, 188);
        }

        .nextBtn:hover .rightArrow,
        .previousBtn:hover .leftArrow {
          color: rgb(116, 76, 188);
        }
      `}
    >
      {previous ? (
        <Link to={previous.url} className={'previousBtn'}>
          <div className={'leftArrow'}>
            <svg
              preserveAspectRatio="xMidYMid meet"
              height="1em"
              width="1em"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 0 24 24"
              strokeWidth="2"
              strokeLinecap="round"
              strokeLinejoin="round"
              stroke="currentColor"
              className="_13gjrqj"
            >
              <g>
                <line x1="19" y1="12" x2="5" y2="12" />
                <polyline points="12 19 5 12 12 5" />
              </g>
            </svg>
          </div>
          <div className={'preRightWrapper'}>
            <div className={'smallContent'}>
              <span>Previous</span>
            </div>
            <div className={'nextPreviousTitle'}>
              <span>{previous.title}</span>
            </div>
          </div>
        </Link>
      ) : null}
      {next ? (
        <Link to={next.url} className={'nextBtn'}>
          <div className={'nextRightWrapper'}>
            <div className={'smallContent'}>
              <span>Next</span>
            </div>
            <div className={'nextPreviousTitle'}>
              <span>{next.title}</span>
            </div>
          </div>
          <div className={'rightArrow'}>
            <svg
              preserveAspectRatio="xMidYMid meet"
              height="1em"
              width="1em"
              fill="none"
              xmlns="http://www.w3.org/2000/svg"
              viewBox="0 0 24 24"
              strokeWidth="2"
              strokeLinecap="round"
              strokeLinejoin="round"
              stroke="currentColor"
              className="_13gjrqj"
            >
              <g>
                <line x1="5" y1="12" x2="19" y2="12" />
                <polyline points="12 5 19 12 12 19" />
              </g>
            </svg>
          </div>
        </Link>
      ) : null}
    </div>
  );
}

