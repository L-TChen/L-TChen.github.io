<?xml version="1.0"?>
<!-- Site-owned composition root for the pinned Forester 5.0 base theme. -->
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:f="http://www.forester-notes.org">

  <xsl:output method="html" encoding="utf-8" indent="yes" doctype-public="" doctype-system="" omit-xml-declaration="yes"/>
  <xsl:strip-space elements="f:author f:contributor"/>

  <!-- Forester's structural renderers. -->
  <xsl:include href="core.xsl" />
  <xsl:include href="metadata.xsl" />
  <xsl:include href="links.xsl" />
  <xsl:include href="tree.xsl" />

  <!-- Site layout, mirroring the Hakyll template layers. -->
  <xsl:include href="head.xsl" />
  <xsl:include href="navbar.xsl" />
  <xsl:include href="main.xsl" />
  <xsl:include href="footer.xsl" />
  <xsl:include href="scripts.xsl" />

  <xsl:template match="/">
    <html xmlns="http://www.w3.org/1999/xhtml" lang="en" data-bs-theme="auto" data-base-url="{/f:tree/@base-url}">
      <xsl:call-template name="site-head" />
      <body>
        <ninja-keys placeholder="Start typing a note title or ID"></ninja-keys>
        <xsl:call-template name="site-navbar" />
        <main role="main" class="forester-shell site-main">
          <xsl:call-template name="site-main-content" />
        </main>
        <xsl:call-template name="site-footer" />
        <xsl:call-template name="site-scripts" />
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
