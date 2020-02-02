import React from 'react';
import {Link} from "./Link";

export const NextPrevious = (props) => {
  const { nav } = props;
  console.log(nav)
    let currentIndex;
    let nextInfo;
    let previousInfo;
    if (currentIndex === undefined) {
      // we are on the index page
      if(nav[0]) {
        nextInfo = nav[0];
      }
      previousInfo = null;
      currentIndex = -1;
    } else if (currentIndex === 0) {
      nextInfo = nav[currentIndex+1]
      previousInfo = null;
    } else if (currentIndex === (nav.length-1)) {
      nextInfo = null;
      previousInfo = nav[currentIndex-1]
    } else if (currentIndex) {
      nextInfo = nav[currentIndex+1];
      previousInfo = nav[currentIndex-1];
    }
    return (
      <div className={'nextPreviousWrapper'}>
        {previousInfo ? (
          <Link to={previousInfo.url} className={'previousBtn'}>
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
                <span>{previousInfo.title}</span>
              </div>
            </div>
          </Link>
        ) : null}
        {nextInfo ? (
          <Link to={nextInfo.url} className={'nextBtn'}>
            <div className={'nextRightWrapper'}>
              <div className={'smallContent'}>
                <span>Next</span>
              </div>
              <div className={'nextPreviousTitle'}>
                <span>{nextInfo.title}</span>
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

