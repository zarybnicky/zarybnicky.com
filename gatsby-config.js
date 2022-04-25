const path = require('path');
require('dotenv').config()

module.exports = {
  // pathPrefix: `/notes`, // If your Digital Garden is not published at the root of your website, use this.
  trailingSlash: 'never', // Remove all trailing slashes on each URL, e.g. /x/ to /x

  siteMetadata: {
    title: `Gatsby Garden`,
    description: `A Digital Garden tended by Gatsby`,

    // siteUrl: `https://yoursite.com/notes/`, // URL at which your site will be published. This should be present if you want RSS feed.
    headerMenu: [
      { type: 'page', item: '', title: 'Home' }, // Type can be 'page', 'note', 'tag', or 'link'
      { type: 'page', item: 'sitemap', title: 'Sitemap' },
      // {type: 'page', item: 'rss.xml', title: 'RSS'},
      {
        type: 'page', item: 'tags', title: 'Tags',
        // menu: [
        //   {type: 'tag',item: 'zettelkasten'},
        //   {type: 'tag',item: 'philosophy'},
        //   {type: 'tag',item: 'psychology'},
        //   {type: 'tag',item: 'rationality'},
        // ]
      },
    ],

    menu: [
      { type: 'page', item: '', title: 'Home' }, // Type can be 'page', 'note', 'tag', or 'link'
      { type: 'page', item: 'sitemap', title: 'Sitemap' },
      { type: 'page', item: 'tags', title: 'Tags' },
    ],
  },

  plugins: [
    `gatsby-plugin-catch-links`,

    // { // Enable this if you want to have an RSS Feed. The `siteMetadata.siteUrl` property should be present for this to work
    //   resolve: `gatsby-plugin-feed`,
    //   options: {
    //     query: `
    //       {
    //         site {
    //           siteMetadata {
    //             title
    //             description
    //             siteUrl
    //             site_url: siteUrl
    //           }
    //         }
    //       }
    //     `,
    //     feeds: [
    //       {
    //         serialize: ({ query: { site, allMarkdownRemark } }) => {
    //           return allMarkdownRemark.edges.map(edge => {
    //             return Object.assign({}, edge.node.fields, {
    //               description: edge.node.excerpt,
    //               date: edge.node.fields.date,
    //               url: site.siteMetadata.siteUrl + edge.node.fields.slug,
    //               guid: site.siteMetadata.siteUrl + edge.node.fields.slug,
    //               custom_elements: [{ "content:encoded": edge.node.html }]
    //             });
    //           });
    //         },
    //         query: `
    //           {
    //             allMarkdownRemark(
    //               limit: 20,
    //               sort: { order: DESC, fields: [fields___date] },
    //             ) {
    //               edges {
    //                 node {
    //                   excerpt
    //                   html
    //                   fields { slug date title }
    //                 }
    //               }
    //             }
    //           }
    //         `,
    //         output: "/rss.xml",
    //         title: "RSS Feed",
    //       }
    //     ]
    //   }
    // },

    {
      resolve: `gatsby-source-filesystem`,
      options: {
        name: `notes`,
        path: path.resolve(process.env.VAULT_LOCATION),
        ignore: [`**/\.*`],
      },
    },

    {
      resolve: `gatsby-transformer-remark`,
      options: {
        plugins: [
          'gatsby-remark-copy-linked-files',
          {
            resolve: 'gatsby-remark-graph',
            options: {
              language: 'mermaid',
              theme: 'neutral',
            }
          },
          {
            resolve: 'gatsby-remark-obsidian',
            options: {
              titleToURL: require(`${__dirname}/src/utils/make-slug.js`)
            }
          }
        ],
      },
    },

    {
      resolve: `gatsby-plugin-typescript`,
      options: {
        isTSX: true,
        allExtensions: true,
      },
    },
    'gatsby-plugin-typescript-checker',

    {
      resolve: `gatsby-plugin-purgecss`,
      options: {
        // printRejected: true, // Print removed selectors and processed file names. Use for debugging.
        // develop: true, // Enable while using `gatsby develop`
        // tailwind: true, // Enable tailwindcss support
        ignore: ['tippy.js/', 'tooltip.css'], // Ignore files/folders
        // purgeOnly : ['components/', '/main.css', 'bootstrap/'], // Purge only these files/folders
        purgeCSSOptions: {
          safelist: ['.tippy-box'], // Don't remove this selector
        },
      },
    },

    {
      resolve: 'gatsby-plugin-local-search',
      options: {
        name: 'notes_index',
        engine: 'flexsearch',
        engineOptions: {
          present: 'speed',
          tokenize: 'forward'
        },
        query: `
          {
            allMarkdownRemark(filter: { fields: { visibility: { eq: "public" } } }) {
              nodes {
                id
                fields {
                  title
                  slug
                  excerpt
                }
                frontmatter {
                  tags
                }
                rawMarkdownBody
                excerpt
              }
            }
          }
        `,
        ref: 'id',
        index: ['title', 'body', 'tags'],
        store: ['id', 'slug', 'title', 'excerpt'],
        normalizer: ({ data }) => data.allMarkdownRemark.nodes.map(node => ({
          id: node.id,
          slug: node.fields.slug,
          title: node.fields.title,
          excerpt: node.fields.excerpt ? node.fields.excerpt : node.excerpt,
          tags: node.frontmatter.tags,
          body: node.rawMarkdownBody,
        })),
      },
    },
  ],
}
