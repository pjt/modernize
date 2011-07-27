<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet 
	version="2.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:lookup="java:Lookup"
	xmlns="http://www.tei-c.org/ns/1.0"
   exclude-result-prefixes="lookup">

  <!--
	  ============================================================
     Testing Saxon's Extension Funcs in Java
	  Perry Trolard
	  Humanities Digital Workshop
     Sun 18 Jan 2009
	  ============================================================
  -->

  <xsl:output method="xml" encoding="utf-8" indent="yes"/>

  <!--
	  ============================================================
	  As default, copy element, att, comment, and text nodes.
	  ============================================================
  -->

  <!-- default to copying element nodes, processing att nodes -->
  <xsl:template match="node()|@*">
	<xsl:copy>
	  <xsl:apply-templates select="@*"/> 
	  <xsl:apply-templates/>
	</xsl:copy>
  </xsl:template>

  <!-- override default ignoring of processing instructions -->
  <xsl:template match="processing-instruction()">
	<xsl:copy-of select="."/>
  </xsl:template>

  <!-- default behavior is to copy text nodes -->

  <!--
   Scan text nodes for modernization replacements
   ==============================================
  -->
  <xsl:template match="//*:text//text()">
   <xsl:analyze-string select="." regex="\w+">
     <xsl:matching-substring>
      <xsl:choose>
        <xsl:when test="lookup:contains(.)">
          <choice>
             <orig><xsl:value-of select="."/></orig>
             <reg type="modernize"><xsl:value-of select="lookup:get(.)"/></reg>
          </choice>
        </xsl:when>
        <xsl:otherwise>
         <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:matching-substring>
    <xsl:non-matching-substring>
      <xsl:value-of select="."/>
    </xsl:non-matching-substring>
   </xsl:analyze-string>
  </xsl:template>

  <!--
       Special case for text nodes already in <orig>s
       ==============================================
  -->
  <xsl:template match="*:orig//text()">
   <xsl:copy/>
  </xsl:template>

  <!--
       Special case for text nodes already in <reg>s
       ==============================================
  -->
  <xsl:template match="*:reg[text()]">
   <xsl:choose>
    <xsl:when test="some $t in tokenize(string(), '\W') satisfies lookup:contains($t)">
     <xsl:copy>
      <xsl:apply-templates select="@* except @type"/>
      <xsl:attribute name="type">modernize<xsl:value-of select="replace(@type, 'modernize', '')"/>
         </xsl:attribute>
      <xsl:apply-templates/>
     </xsl:copy>
    </xsl:when>
    <xsl:otherwise>
     <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
     </xsl:copy>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:template>

  <xsl:template match="*:reg//text()">
   <xsl:analyze-string select="." regex="\w+">
     <xsl:matching-substring>
      <xsl:choose>
        <xsl:when test="lookup:contains(.)">
           <xsl:value-of select="lookup:get(.)"/>
        </xsl:when>
        <xsl:otherwise>
         <xsl:value-of select="."/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:matching-substring>
    <xsl:non-matching-substring>
      <xsl:value-of select="."/>
    </xsl:non-matching-substring>
   </xsl:analyze-string>
  </xsl:template>

</xsl:stylesheet>
