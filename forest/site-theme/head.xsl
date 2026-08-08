<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:f="http://www.forester-notes.org">

  <xsl:template name="site-head">
    <head xmlns="http://www.w3.org/1999/xhtml">
      <meta charset="utf-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <meta name="theme-color" content="#6c757d" />
      <script src="/src/bootstrap-auto-dark-mode.js"></script>
      <link rel="preconnect" href="https://fonts.googleapis.com" />
      <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin="anonymous" />
      <link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Bona+Nova:ital,wght@0,400;0,700;1,400&amp;family=LXGW+WenKai+Mono+TC&amp;display=swap" />
      <script async="async" src="https://kit.fontawesome.com/d25dd1f650.js" crossorigin="anonymous"></script>
      <link rel="stylesheet" href="{/f:tree/@base-url}style.css" />
      <link rel="stylesheet" href="/css/default.css" />
      <link rel="stylesheet" href="{/f:tree/@base-url}katex.min.css" />
      <link rel="stylesheet" href="/css/forester.css" />
      <script type="text/javascript">
        <xsl:if test="/f:tree/f:frontmatter/f:source-path">
          <xsl:text>window.sourcePath = '</xsl:text>
          <xsl:value-of select="/f:tree/f:frontmatter/f:source-path" />
          <xsl:text>'</xsl:text>
        </xsl:if>
      </script>
      <script type="module" src="{/f:tree/@base-url}forester.js"></script>
      <title>
        <xsl:value-of select="/f:tree/f:frontmatter/f:title/@text" />
      </title>
    </head>
  </xsl:template>
</xsl:stylesheet>
