import React from "react";
import CodeBlock from "./codeBlock";
import AnchorTag from "./anchor";

export default {
  h1: props => (
    <h1
      className="heading1"
      id={props.children.replace(/\s+/g, '').toLowerCase()}
      {...props}
    />
  ),
  h2: props => (
    <h2
      className="heading2"
      id={props.children.replace(/\s+/g, '').toLowerCase()}
      {...props}
    />
  ),
  h3: props => (
    <h3
      className="heading3"
      id={props.children.replace(/\s+/g, '').toLowerCase()}
      {...props}
    />
  ),
  h4: props => (
    <h4
      className="heading4"
      id={props.children.replace(/\s+/g, '').toLowerCase()}
      {...props}
    />
  ),
  h5: props => (
    <h5
      className="heading5"
      id={props.children.replace(/\s+/g, '').toLowerCase()}
      {...props}
    />
  ),
  h6: props => (
    <h6
      className="heading6"
      id={props.children.replace(/\s+/g, '').toLowerCase()}
      {...props}
    />
  ),
  p: props => <p className="paragraph" {...props} />,
  pre: props => <pre className="pre" {...props} />,
  code: CodeBlock,
  a: AnchorTag,
  img: props => <img className="img" {...props} />,
  blockquote: props => <blockquote className="img" {...props} />,
}
