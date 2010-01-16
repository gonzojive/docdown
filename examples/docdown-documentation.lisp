(in-package :docdown)

(declaim (optimize (debug 3)))

(defdoc index :page
  (:title "DocDown")
  (:content
   "
## Introduction

Docdown is a tool for generating unimposing documentation for Common
Lisp code taht looks like the current page.

Inspired by [DOCUMENTATION-TEMPLATE](http://weitz.de/documentation-template/), Docdown
avoids spaghetti documentation with a lot of clicking around and
cross-reference in favor of documentation that is meant to be read
from beginning to end with occasional flipping around.  Nonetheless,
hyperlinks exist for webifying documentation at will.

Example:

    (defdoc index :page
      (:title \"DocDown\")
      (:content \"## Introduction
    
    Docdown is a tool for generating unimposing documentation for Common Lisp code...\"))

Documentation takes the form of a graph of CLOS objects connected by
slots as edges.
"
#+nil
"
## Table of Contents

-  First table of contents item
    - sublist
    - sublist 2
-  Second table of contents item
-  Third table of contents item

")
  (:sections
   (defdoc tutorial :section
     (:title "Tutorial")
     (:content
      "

Documentation takes the form of a graph structure, and it is currently
defined with a few Common Lisp macros and functions, though in theory
the graph of nodes could be read in from another source.  Generation
of HTML output from a DocDown graph generally starts at a 'page' node:

    (defdoc index :page
      (:title \"Example Project\")
      (:content \"## Introduction
    
    Here we describe the example project in a few paragraphs.  They
    are parsed using [CL-Markdown](http://common-lisp.net/project/cl-markdown/)\"))

The above code defines a node called `index` of with a documentation
type `:PAGE`, title `\"Example Project\"` and markdown content.
Although `index` is the only node defined explicitly with `defdoc`,
`\"Example Project\"` and the markdown content are also nodes in the
documentation.  The symbol `INDEX` names the page node defined above,
but the other nodes have no name.

DocDown generates documentation by passing over the documentation
graph from a particular start node with a call to the function `DOC`
with a particular \"contextual layer\" active:

    (contextl:with-active-layers (docdown:html-generation-layer)
      (docdown:doc 'index))
    ==>
    \"<html>...</html>\"    


Although the documentation is technically a graph, it is often raked
over as a tree in order to generate the documentation.  DocDown docs
are intended to be mostly readable linearly, and it is certainly
composed on the page in a rather linear fashion.  So when you compose
DocDown, it is also a fairly linear process.

DocDown documentation may occur inline with lisp code, or it may be in
a separate file, or it may use a combination of the two.  All
documentation is currently Common Lisp code itself--it is not stripped
out of ;;;-style comments or anything fancy.



"))

    (defdoc html-generation :section
      (:title "HTML Generation")
      (:content
       "HTML Generation occurs when the `DOC` function passes over the
docdown graph with the `HTML-GENERATION-LAYER` active.  ")
      (:children
       (defdoc generate-html-page :function
	 (:subject-symbol 'generate-html-page)
	 (:content
	  "Given a page docnode , generates a sting of HTML"))
       (defdoc doc :function
	 (:subject-symbol 'doc)
	 (:content
	  "Given a docnode, generates some sort of documentation for
it.  Behavior depends on the active ContextL layers."))))))

(defun output-documentation ()
  (with-open-file (stream "doc.html" :direction :output :if-exists :supersede)
    (write-string (generate-html-page 'index) stream)))

(defdoc generate-html-page :function
  (:content
   "Given a page docnode , generates a sting of HTML"))