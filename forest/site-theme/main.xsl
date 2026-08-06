<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:f="http://www.forester-notes.org">

  <!-- Inner post layout only; the composition root owns its placement and padding. -->
  <xsl:template name="site-main-content">
    <xsl:variable name="has-toc" select="f:tree/f:mainmatter/f:tree[not(@toc='false')] and not(f:tree/f:frontmatter/f:meta[@name='toc']/.='false')" />
    <div xmlns="http://www.w3.org/1999/xhtml" class="site-content-layout">
      <article class="site-primary-column">
        <xsl:apply-templates select="f:tree" />
      </article>
      <xsl:if test="$has-toc">
        <nav id="toc" class="site-rail" aria-label="Table of contents">
          <div class="block site-panel site-panel--small">
            <p class="site-panel-title">Table of Contents</p>
            <xsl:apply-templates select="f:tree/f:mainmatter" mode="toc" />
          </div>
        </nav>
      </xsl:if>
    </div>
  </xsl:template>
</xsl:stylesheet>
