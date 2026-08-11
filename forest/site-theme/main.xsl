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

  <!--
    The base theme uses h1 for every tree, including nested subtrees and
    generated backmatter. Keep its content rendering while deriving the
    semantic heading level from the tree depth.
  -->
  <xsl:template match="f:frontmatter" priority="1">
    <xsl:variable name="tree-depth" select="count(ancestor::f:tree)" />
    <xsl:variable name="heading-level">
      <xsl:choose>
        <xsl:when test="$tree-depth &gt; 6">6</xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$tree-depth" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <header>
      <xsl:element name="{concat('h', normalize-space($heading-level))}">
        <xsl:attribute name="class">forester-heading</xsl:attribute>
        <span class="taxon">
          <xsl:apply-templates select=".." mode="tree-taxon-with-number">
            <xsl:with-param name="suffix">.&#160;</xsl:with-param>
          </xsl:apply-templates>
        </span>

        <xsl:apply-templates select="f:title" />
        <xsl:text>&#032;</xsl:text>
        <xsl:apply-templates select="f:display-uri" />
        <xsl:text>&#032;</xsl:text>
        <xsl:apply-templates select="f:source-path" />
      </xsl:element>
      <div class="metadata">
        <ul>
          <xsl:apply-templates select="f:date" />
          <xsl:if test="not(f:meta[@name = 'author']/.='false')">
            <xsl:apply-templates select="f:authors" />
          </xsl:if>
          <xsl:apply-templates select="f:meta[@name='position']" />
          <xsl:apply-templates select="f:meta[@name='institution']" />
          <xsl:apply-templates select="f:meta[@name='venue']" />
          <xsl:apply-templates select="f:meta[@name='source']" />
          <xsl:apply-templates select="f:meta[@name='doi']" />
          <xsl:apply-templates select="f:meta[@name='orcid']" />
          <xsl:apply-templates select="f:meta[@name='external']" />
          <xsl:apply-templates select="f:meta[@name='slides']" />
          <xsl:apply-templates select="f:meta[@name='video']" />
        </ul>
      </div>
    </header>
  </xsl:template>
</xsl:stylesheet>
