import React from 'react'
import { useStaticQuery, graphql, Link } from 'gatsby'
import { Helmet } from 'react-helmet'
import { startCase, camelCase } from 'lodash'
import Search from '../components/search'
import {
  DefaultMenuStructure,
  MenuItemPage,
  MenuItemText,
  MenuItemNote,
  MenuItemTag,
  MenuItemExternalLink,
} from '../utils/menu-structure'

export const Layout: React.FC<{
  children: React.ReactNode;
  title?: string;
  type?: 'note' | 'tag' | 'home';
  description?: string;
}> = ({ children, title, type, description }) => {
  const { site } = useStaticQuery(
    graphql`
      query {
        site {
          siteMetadata {
            title
          }
        }
      }
    `
  );

  const menu = DefaultMenuStructure('header')
  const pageTitle =
    (site.siteMetadata.title || 'Gatsby Garden') +
    (title ? ` : ${title}` : '')

  const handleHamburgerMenuClick = e => {
    const target_id = e.currentTarget.getAttribute('data-target')
    const target = document.getElementById(target_id)

    // Toggle the "is-active" class on both the "navbar-burger" and the "navbar-menu"
    e.currentTarget.classList.toggle('is-active')
    target?.classList?.toggle('is-active')
  }

  // :TODO:
  // <meta content="{{ site.url }}{{ page.url }}" property="og:url"> - NOTE: site.url might NOT be there in the config file.

  return (
    <>
      <Helmet>
        <meta content="width=device-width, initial-scale=1" name="viewport" />
        <meta content={site.siteMetadata.title} property="og:site_name" />
        <meta content={title || pageTitle} property="og:title" />
        {description ? <meta content={description} property="og:description" /> : null}
        <meta content={type === 'note' ? "article" : 'website'} property="og:type"></meta>
        <title>{pageTitle}</title>
        <link rel="apple-touch-icon" href="/favicon.png" />
        <link rel="icon" href="/favicon.png" type="image/png" sizes="16x16" />
        {/*
          These are included using gatsby-browser.js - if I include these like shown here, there is a horrible FOUC
        <link href="/css/style.css" rel="stylesheet" media="all" className="default" />
        <link href="/css/main.css" rel="stylesheet" media="all" className="default" />
        <link href="/css/custom.css" rel="stylesheet" media="all" className="default" />
        <link href="/css/Util.css" rel="stylesheet" media="all" className="default" />
        */}
      </Helmet>

      <nav
        className="navbar is-transparent"
        role="navigation"
        aria-label="main navigation"
      >
        <div className="navbar-brand">
          <Link className="navbar-item" to="/">
            <img alt="" src="/img/seagull.png" style={{ paddingRight: '.5rem' }} />
            <h4>{site.siteMetadata.title || 'Gatsby Garden'}</h4>
          </Link>
          <button
            className="navbar-burger button-link"
            aria-label="menu"
            aria-expanded="false"
            data-target="navbar-main"
            onClick={handleHamburgerMenuClick}
          >
            <span aria-hidden="true"></span>
            <span aria-hidden="true"></span>
            <span aria-hidden="true"></span>
          </button>
        </div>

        <div className="navbar-menu" id="navbar-main">
          <div className="navbar-start">
            {menu.map((item, index) => item?.['menu'] ? (
              <span key={index} className="navbar-item dropdown">
                <Link
                  to={`/${item.item}`}
                  id={`dropdown-${item.item}`}
                  className="dropdown-toggle"
                  data-toggle="dropdown"
                  aria-haspopup="true"
                  aria-expanded="false"
                >
                  {item.title || startCase(camelCase(item.item))}
                </Link>
                <div
                  className="dropdown-menu"
                  aria-labelledby={`dropdown-${item.item}`}
                >
                  {item['menu'].map((subItem, i) => (
                    <MenuItem className="navbar-item" item={subItem} key={i} />
                  ))}
                </div>
              </span>
            ) : (
              <MenuItem className="navbar-item" item={item} key={index} />
            ))}
          </div>
          <div className="navbar-end is-hidden-mobile">
            <div className="navbar-item">
              <Search size="small" showExcerpt={false} />
            </div>
          </div>
        </div>
      </nav>

      <section className="section">
        <div className="columns is-centered">{children}</div>
      </section>
    </>
  )
}

function MenuItem({ item, className }) {
  let itm
  if (item.type === 'page')
    itm = <MenuItemPage item={item} className={className} />
  else if (item.type === 'tag')
    itm = <MenuItemTag item={item} className={className} />
  else if (item.type === 'note')
    itm = <MenuItemNote item={item} className={className} />
  else if (item.type === 'link')
    itm = <MenuItemExternalLink item={item} className={className} />
  else if (item.type === 'text')
    itm = <MenuItemText item={item} className={className} />

  return itm
}
