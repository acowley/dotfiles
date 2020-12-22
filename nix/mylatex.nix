self: super: {
  mylatex = super.texlive.combine {
     inherit (super.texlive) scheme-small algorithms cm-super
             collection-binextra collection-context
             collection-fontsrecommended collection-fontutils
             collection-mathscience collection-formatsextra
             collection-pictures subfigure web supertabular
             wrapfig capt-of footmisc subdepth preview
             minifp lettrine titling titlesec fontspec todonotes
             doublestroke comment multirow cancel doi import ifoddpage
             subfiles biblatex logreq biber was minted fvextra xstring
             framed standalone aeguill catchfile pagecolor fontawesome;
    };
}
