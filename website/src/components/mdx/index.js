import React from "react";
import CodeBlock from "./codeBlock";
import AnchorTag from "./anchor";
import { formatTitleToId } from '../../id';

export default {
  h1: props => (
    <h1 className="heading1" id={formatTitleToId(props.children)} {...props} />
  ),
  h2: props => (
    <h2 className="heading2" id={formatTitleToId(props.children)} {...props} />
  ),
  h3: props => (
    <h3 className="heading3" id={formatTitleToId(props.children)} {...props} />
  ),
  h4: props => (
    <h4 className="heading4" id={formatTitleToId(props.children)} {...props} />
  ),
  h5: props => (
    <h5 className="heading5" id={formatTitleToId(props.children)} {...props} />
  ),
  h6: props => (
    <h6 className="heading6" id={formatTitleToId(props.children)} {...props} />
  ),
  p: props => <p className="paragraph" {...props} />,
  pre: props => <pre className="pre" {...props} />,
  code: CodeBlock,
  a: AnchorTag,
  img: props => <img className="img" {...props} />,
  blockquote: props => <blockquote className="blockquote" {...props} />,
};
