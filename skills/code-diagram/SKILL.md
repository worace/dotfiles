---
name: code-diagram
description: Generate an execution flow diagram of an area of code in Figma.
---

Generate a code execution flow diagram _in Figma_ of the area of code described by the user. The goal here is to show a visual representation of the structural tree of some piece of code. We want to capture function chains, logical branching, class or module boundaries, things that give a high level traceable visualization of how something works. We're looking for mostly boxes and arrows, boxes to indicate a code unit like a function, class, or module, and arrows to indicate a jump point from one to another. The most common and important flow to capture is function chaining - that's the most valuable flow to make traceable in these diagrams. But where useful capturing other code primitives like class or module boundaries, 

Use the related /figma-use-figjam skill to understand conventions for working with Figma as a diagramming tool

Some conventions to use when building the diagram:

* Make code references in the diagram hyperlinks to a corresponding github Permalink in the github web UI. The goal is to make each code reference clickable to view its definition in github.
* Typographically, make function names use the "Technical" font in Figjam, and make them left-aligned (typically figjam diagram nodes are often center aligned by default)
* Where useful, use a Figjam "Section" to aggregate subsections of code flow within a larger diagram. For example using a Section to group all of the code within a given Class, Module, Agent, or Tool can be a useful abstraction primitive
