import React, { useState } from 'react';
import { createGlobalStyle } from 'styled-components'
import {Opened, Closed} from '../Icon';
import { Link } from '../Link';

let TreeStyles = createGlobalStyle`

.showFrontLine .item >a:hover {
  background-color: #542683;
}
.showFrontLine .active >a {
  color: #fff;
  background-color: #473485;
}

.showFrontLine  .item .item {
  border-left: 1px solid #e6ecf1;
  border-left-color: rgb(230, 236, 241);
  padding: 0;
  width: calc(100% - 16px);
}

.showFrontLine .item .active>a {
  border-color: rgb(230,236,241);
  border-style: solid none solid solid;
  border-width: 1px 0px 1px 1px;
  background-color: #542683;
  color: #fff;
}



.firstLevel ul li .collapser svg path {
  fill: #fff;
}
.active .collapser >svg >path {
  fill: #663399;
}

.firstLevel ul .item ul .item {
  border-left: 1px solid #e6ecf1;
}

.firstLevel>ul>.item{
  margin-left: 0;
}
`;

const TreeNode = ({
  className = '',
  setCollapsed,
  collapsed,
  url,
  title,
  items,
}) => {
  const isCollapsed = collapsed[url];
  const hasChildren = items.length !== 0;  
  const active = false;
  return (
    <li className={`${className} item ${active ? 'active' : ''}`}>
      {title && (
        <Link to={url}>
          {title}
          {hasChildren ? (
            <button
              onClick={() => {
    setCollapsed(url)
  }}
              aria-label="collapse"
              className="collapser"
            >
              {!isCollapsed ? <Opened /> : <Closed />}
            </button>
          ) : null}
        </Link>
      )}

      {!isCollapsed && hasChildren ? (
        <ul>
          {items.map(item => (
            <TreeNode
              key={item.url}
              setCollapsed={setCollapsed}
              collapsed={collapsed}
              {...item}
            />
          ))}
        </ul>
      ) : null}
    </li>
  );
};

const calculateTreeData = edges => {
  const originalData = edges;
  const tree = originalData.reduce((accu, {node: {frontMatter, fields: {url, title}}}) => {
    const parts = url.split('/');
    let {items: prevItems} = accu;
    for (const part of parts.slice(1, -1)) {
      let tmp = prevItems.find(({label}) => label == part);
      if (tmp) {
        if (!tmp.items) {
          tmp.items = [];
        }
      } else {
        tmp = {label: part, items: []};
        prevItems.push(tmp)
      }
      prevItems = tmp.items;
    }
    const existingItem = prevItems.find(({label}) => label === parts[parts.length - 1]);
    if (existingItem) {
      existingItem.url = url;
      existingItem.title = title;
    } else {
      prevItems.push({
        label: parts[parts.length - 1],
        url: url,
        items: [],
        title
      });
    }
    return accu;
  }, {items: []});
  return [].reduce((accu, url) => {
    const parts = url.split('/');
    let {items: prevItems} = accu;
    for (const part of parts.slice(1, -1)) {
      let tmp = prevItems.find(({label}) => label == part);
      if (tmp) {
        if (!tmp.items) {
          tmp.items = [];
        }
      } else {
        tmp = {label: part, items: []};
        prevItems.push(tmp)
      }
      prevItems = tmp.items;
    }

    // TODO sort by order frontmatter then alphabetically.
    prevItems.map((item) => {
      item.items = item.items
        .sort(function (a, b) {
          if (a.label < b.label)
            return -1;
          if (a.label > b.label)
            return 1;
          return 0;
        });
    })
    const index = prevItems.findIndex(({label}) => label === parts[parts.length - 1]);
    accu.items.unshift(prevItems.splice(index, 1)[0]);
    return accu;
  }, tree);
}

export const Tree = ({edges}) => {
  const treeData = calculateTreeData(edges);
  const [collapsed, setCollapsed] = useState({});
  const toggle = (url) => {
    setCollapsed({
      ...collapsed,
      [url]: !collapsed[url],
    });
  }
  return (
    <>
      <TreeStyles />
    <TreeNode
      className={'showFrontLine firstLevel'}
      setCollapsed={toggle}
      collapsed={collapsed}
      {...treeData}
      />
      </>
  );
}
