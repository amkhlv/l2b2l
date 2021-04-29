@bold{This} is an example
@(itemlist
  #:style 'ordered
  @item{first item:
   @(itemlist @item{a @bold{bold}} @item{b @italic{it}} @item{c @tt{tt}})}
  @item{second item}
  )
--- this is a nested list!

Now start new paragraph

@subpage[1 "First section"]

@subpage[2 "Tables"]

Tables @bold{do not work} in @tt{docx} :

@(tbl (list (list "head1" "head2") (list "td21" "td22")))

@(hyperlink "https://stackoverflow.com/questions/17858598/add-styling-rules-in-pandoc-tables-for-odt-docx-output-table-borders" "discussed in this Stack Overflow question")

@subpage[2 "Formulas"]

Inline math formula: @f{\int dx dy(x^2 + y^2)}
and display math:

@e{m\dot{x}^2}

@subpage[1 "Concluding section"]
