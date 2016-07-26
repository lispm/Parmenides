@make(article)
@comment[@libraryfile (isllib)]
@style(spacing 1.1)
@modify[description, leftmargin=.25 in, indent=.25 in]
@pageheading(odd, left "@c[PARMENIDES Manual]", right "@c[Page @ref(page)]")
@pageheading(even, left "@c[Page @ref(page)]")
@comment{ center "@c[@title(chapter)]", }
@begin(COMMENT)
To clarify:

Document inverse relations with more examples.

Add set-slot-demons

@end(COMMENT)
@Begin (Titlepage)
@Begin (Titlebox)
@blankspace[1 in]
@MajorHeading (PARMENIDES: A Class-Based Frame System)
@End(Titlebox)
@Heading (Version 1.5)
@Value (date)
@blankspace (1in)
@b "Peter Shell"
@b "Jaime Carbonell"

@copyrightnotice[Carnegie Mellon University]
@End (Titlepage)

@Section (Introduction)

  @b(PARMENIDES) (PropagAting, Rulekit-Motivated ENgine for Instance
DEScriptions) is a
frame-based knowledge representation system.
It is influenced by @b(SRL), @b(Framekit),
@b(CommonLoops), and the Spice lisp structure implementation.
The slot and facet access functions have comparable speed to the Slisp slot
access functions.
It has some of the Framekit and SRL functionality, such as
facets, demons, and user-defined relations.  However,
like Loops, it makes a distinction between classes and instances.
This means that instances can only be
an instance of one class, while classes may be subclasses of
(have an @i(is-a) relation with) more than one class.
Instances may not have any slots that their classes don't have.
Classes describe a way to make instances,
and instances only participate in the frame network through their classes.
By storing much of the frame information at the class level, the amount
of storage needed for instances is reduced.
For now, classes can not also be instances, but this could be implemented
in the future.  Parmenides is implemented in Common Lisp, so it is assumed that
the user is familiar with this dialect of Lisp.

  The name is in recognition of Plato's @i(Parmenides) dialogue, in which he
is confronted with the "third person argument".
This is the notion that
in order to describe and generate instances through the
generative powers of classes,  the classes in turn need a meta-class
to generate them, and so on.  This argument is used as an excuse not
to implement meta-classes in this frame system.

  Other features of @b[Parmenides] include:
@begin(itemize)

@i(Propagation) instead of @i(inheritance).  See the section on propagation
for more details.

@i(Optional facets).  Usually, slots contain facets, which can be thought
of as annotated property lists of a slot.  For example, one might want
to store
both a value and degree of certainty with some slots.  However, if only
one value and no facets need be stored,
then the slot may be declared to not have facets,
thus making access to and representation of that slot more efficient.

@i(Extendable Frames).  Since frames are represented as adjustable arrays, it is
possible
to add slots and facets dynamically (i.e., after the class has been defined).

@i(Class slots).  @b(Parmenides) supports class slots, which are
ways to specify meta-information about
the classes, and apply not to any particular instance or slot,
but the class itself.  For example, the @i(propagate) attribute is
a class slot.

@i(Pre-access) and @i(post-access) demons.  @b(Parmenides) allows demons to
called both before and after access to values.  See the section on demons.

@i[Multiple Language Messages].  Parmenides messages may
be displayed in a number of languages.  The currently supported languages
are English and Spanish, although it is easy to add messages in new languages.
See section @ref[multi-language] for details.

@i[User-Defined Relations].  The @i(instance) and @i(is-a)
relations are special hard-wired relations, but the user may define his/her own
relations as frames.  Inverse relations are also supported.

@i[User frame files].  Files containing @b[def-frame] commands may
be compiled when
a set of frames is known to be stable.  After that, the compiled frame
file may be loaded without having to load in the source frame file,
resulting in considerable loading and executing speed up.

@i[A freelist of frames].  @b[Parmenides] uses a @i[freelist] in
order to more efficiently manage
memory.  Whenever a frame is removed, it is put on the freelist, and
when a new frame is needed, if there is one of the appropriate type
already on the freelist, then that one is recycled.
The function @b[remove-frame], described below,
releases frames to the freelist, which means that you
shouldn't expect to be able to use the frame after calling them.

@end(itemize)

@begin(comment)  a little too much detail about Rulekit

  Many of the design decisions were motivated by the main goal of having a
frame system that @b(FRulekit), a production system, could use efficiently.
These design decisions were:
@begin(itemize)

Propagation of changes through the graph.
Thus, @i(default values) are stored in classes and
given to sub-classes whenever a new class which is-a that class
is defined, and is
given to instances whenever a new instance is made.

Pre-access and Post-access demons.  The way that the FRulekit $modify command
is implemented requires some processing both before and after
a frame is changed.  It may turn out that $modify will be implemented in
such a way that pre- and post-demons are not necessary, but they seem like
a useful tool anyway.

Class variables.  @b[Parmenides] allows variables to be stored in a class, thus
being shared among all instances (but not stored in the instances).  These
variables are not specific to any single slot.
For example, FRulekit associates certain demons with classes
that are is-a WME, so that when any slot in a frame is changed, FRulekit
finds out about it.
@end(itemize)
@end(comment)

@section(Propagation and Caching versus Inheritance)

  Inheritance is a process by which properties of frames are inferred
from other related frames, typically through is-a or instance links.
Many frame and schema languages perform inheritance at the time of retrieval.
This saves space because when values are shared by a whole tree of frames,
they only need to be stored in one place.  For example, the fact that animals
breathe air need only be stored in the animal class frame, and not in
any subclasses, such as mammal or dog, since the breathes slot of a
dog frame can be inherited from the animal frame.
However, retrieving can be very slow if retrieving with inheritance
is performed often.  Also, if other programs
use a frame network as a data base, then if a class is changed, it will
not necessarily know the implications of this change unless it tests
all of its data again.

  For these reasons, an alternative to inheritance,
@i(caching),  of selected slots and values, is allowed in @b[Parmenides].
Caching a value simply means storing that value directly in a frame.
If a slot is designated a @i(cached) slot by a class, then
the slot and its values will be stored in all subclasses of that class.
Thus, instead of performing inheritance at access time,
@b[Parmenides] is
able to retrieve the data directly from the frame.
For example, suppose the @i(animal) class is first defined with a slot
@i[(breathes (value 'air))] and that the @i(breathes) slot is defined to be
@i(cached).
If a @i(dog) frame is defined as @i(is-a animal),
the slot @i[(breathes (value 'air))] is immediately stored in the
@i(dog) frame (unless of course if the dog frame were to provide
its own specification of the @i(breathes) slot).
Whenever an instance of dog is made, that instance
also gets that slot as a default cached value.

  Cached slots are designated by the @i(cache) class slot (see
the description of
the @b(def-frame) command for the details of designating cached slots).
The tradeoff of cached slots
is that it takes more space to cache all values.
Thus,  if a system never need know if
Fido the dog breathes, then it shouldn't specify the breathes slot as cached.
Standard inheritance is still performed if a cached value can't be found in a
frame (see the Command Summary, below).

Under some circumstances,
it may be necessary to change a class definition
after it and its sub-classes have already been defined.
To ensure that the correct
values are always cached, in the event that a class is changed, the
change is propagated to the sub-classes and instances.
There is a distinction between @i(cached) and @i(local).  A cached
value is one that is stored directly in a frame, but which could
have been inherited from somewhere else.  A local value is one which
was originally stored in the frame (i.e., not inherited from somewhere
else).  @b[Parmenides] retains the distinction between cached and local slots
by keeping a special @i(depth) facet on slots that participate in propagation
@foot(That is, slots which have facets and whose class have the
@i(propagate) property.).  The depth facet
indicates from how far up in the frame hierarchy the value was inherited.
When the depth is 0,
the value is local.   If a slot has a depth which is less
than the depth of a slot which is trying to propagate a new value to it,
then that value won't be propagated to it.
The @i(local-p) function tells whether or not
a certain slot has a local value.

  Propagation is done according to the global system flag !!inheritance-type,
so that when a cached value is retrieved, it will be the same as if
the specified inheritance type was performed.  !!Inheritance-type must be
a keyword, and its value may be either
@i(:dfs) (depth-first) or @i(:bfs) (breadth-first).
It is initially set to @i(:dfs).
(Note that currently @i[:bfs] is not currently implemented).

@Section "Parmenides Commands"

  @b(Parmenides) commands are divided into the following categories:
Class definition, instance creation, data
retrieval, data storage, predicates, and miscellaneous commands.

@subsection(Class Definition Commands)

  It is necessary to define a class before making frame instances, since
classes are prototypes of instances.

@Begin (description)
@p(def-frame  <name> <cplist> &rest <slot-descriptions>)
Top level frame definition function.  Defines a frame class of name @i(<name>)
and makes a default
instance of it with the given default values.  Default values are
evaluated at run time.
Defines a make function for that class, defines
slot access functions for each slot,
and defines facet access functions for the first facet (typically
@i(value)) in each slot in @i[<slot-descriptions>].  Optionally defines set
and setf
methods for slots and facets
(see section 7.2 of @b(Common Lisp, the Language) by Guy Steele for
an explanation of setf methods).  The arguments will now be described.

@paragraph(slot-descriptions)
@i(<slot-descriptions>) is a list of slot descriptions, which are of
the form:
@begin(verbatim)
<slot-description> := <slot-name> <slot-contents>
<slot-contents>	:=  <constant> | <facet-plist>
<facet-plist>	:=  '(' {<facet-name> <default-value>}+ ')'
<default-value>	:=  (any lisp object)
<facet-name>	:=  <slot-name>
<slot-name>	:=  :<symbol>
<constant> is any atom, including NIL.
a <constant> in the facet field denotes a facet-less slot.
Note: constants are not evaluated, but default-values are.

Example of a faceted slot:
:height (:value 'tall :time-inferred 23 :certainty 1.0)

Example of a slot without facets:
:height NIL
@end(verbatim)
A value facet with default value NIL is automatically inserted into
slots that have facets but don't have a value facet.

Note that slot descriptions may be inherited from other classes,
as described in the @i(<is-a>) description, below.

@paragraph(cplist)
@i(<cplist>) is a class plist, which is the way to define class slots.  It
has the same syntax as @i[<slot-descriptions>], and can also be inherited from
super-classes.  Certain class variables are recognized by the system.  For
efficiency, they are all facetless.  These are:

@begin(itemize)

@i(:is-a)  Default: NIL.   An ordered list of class names which the given class
is a
sub-class of.  Slot and other cplist descriptions are inherited from the
superclasses.
When there is more than one description of the same slot or cplist in the
superclass list,
the first such superclass in the @i[is-a]'s list is used.  Local
descriptions take precedence for the @i(is-a) relation.
Because @b(Parmenides) performs propagation instead of inheritance, it
needs to have all the information about the superclasses at class
definition time.  Thus, frame files should define the highest level
frames first, working down.

@i(:cache)  The default is the union of what all the parents cache.
A @i(cache) slot in a class
means that that slot specification is inherited by
any sub-classes of the class.
The value of the @i(cache) class slot,
if specified, should either be a list of the names of the slots to cache,
or the symbol :*ALL*, in which case all slots in the current class will
be cached.  The list of slots to cache is added to what all the parents cache;
thus, cached slots are themselves cached to all the descendants of the class which
defined them.  To override caching what the parents cache, explicitly
specify NIL as the value of this c-slot.

@i(:setable)  Default:  The slots which the parent classess explicitly declare to
be setable.  If the value of this slot is @i[T], then a @b[set]
function will be defined for each slot, analogous to the slot access
functions.  These set functions are named @b[set-]@i[<frame><slot>], e.g.
@b[set-dog-breathes].  Their first argument is the frame to modify, and the
second argument is the new value to put in the slot.  A set function will
also be defined for the @i[value] facet for slots which have facets.
If the value is @i[:setf] then, in addition to the set functions, a
setf method for each slot will be defined.  This allows users the full
power of setf methods (at a slight increase in size of compiled frame
files), such as being able to push onto slots.  Finally, if the value of this
slot is a non-nil list, then the first element of the list should be @i[T] or
@i[:setf], and the second element should be a list containing the slots for which a
@i[set] method is to be defined.  If the parents explicitly declare
slots as settable, then they are also settable by the children; to override
this inheritance, declare the value of @i[:setable] to be NIL.

@i[:getters]  Default: all slots.  Normally, retrieval functions are written
for each slot.  This class slot, whose value must be
a list of slot names, :*ALL* or NIL, may be used to
force @b[Parmenides] to write retrieval functions for only specified slots
and their facets.
It is treated the same way the @i[:cache] facet is treated, i.e., the
keyword :*ALL* is recognized, and it is inherited from super-classes.
This makes @b[Parmenides] more efficient
when there are many frames with many slots, and a retrieval
function is not needed for most slots.

  If you use FRulekit, the @i[:getters] slot should not be altered since
it should normally be :*ALL, which is the default.

@i(:propagate)  Default: T.  If true, then Def-frame will enable the class
to participate in the propagation process when a class is changed.
Since the propagation process requires that the slot that is being changed
has at least a value and depth facet, @b(def-frame) will automatically add these
facets in faceted slots of frames that participate in propagation.  If
propagation is not going to be needed, then turning propagation off will
result in saving space.  If a frame is propagatable, then all of its
parents and descendants
must also be propagatable; otherwise an error will occur.
Note that slots may be cached even when propagation is turned off, since
propagation only takes effect when slots are modified, and the @i(cache)
attribute takes effect when frame classes are defined.

@i(:if-needed)  Default: NIL.  Demons may be specified both as class slots
and as slot facets.  See section @ref[demons], below,
for a description of this and
the next two demon class slots.

@i(:pre-if-set)  Default: NIL.

@i(:post-if-set)  Default: NIL.

@end(itemize)

In addition to the @i(is-a) relation, user-defined relations may be class slot
names.  For example,
if the user defines the @i(part-of) relation and the @i(table) frame,
then in the class slot plist,
@i(part-of table) would be allowed.
Since class slots are meant to modify the definition of the class,
user-defined relations should be put into class slots when they affect which
slots the class would contain.
See the section on
relations, below, for more details.

@paragraph(Discussion)
  Access functions take a frame instance as argument.
Every slot has an associated
slot access function, which has the name @i(<framename>)-@i(<slotname>).
For example, the function @b(person-height) would return the height
of a person instance.
Regardless of whether the slot has facets, the slot access function will return
that slot.  If the slot has no facets, then the value will
be the value stored by the user; otherwise it will be a slot data structure
containing all facets fillers.
If the @i[:setable] class facet is true, then @b[Parmenides] would define
the function @b[set-person-height], which takes a frame instance and value
as arguments, and stores the given value in the @i[:height] slot of the
specified person instance.  If @i[:setable] is set to @i[:setf], then in
addition, the person @i[height] slot is setf-able, e.g., one may perform:
@i[(push 10 (person-height person1))].

  For slots with facets, a slot access function for the first
facet of that slot will be defined
in a way similar to slot functions for frames with no facets, except
facet names will be separated from slots by a "." rather than a dash (to
distinguish slots from facets). For
example, if the @i[size] slot for the house frame had a @i[value] facet, then
the value of size of a house would be retrieved through the
function @i(house-size.value).
If the @i(:setable) class facet is true, then a set function will also be
defined for the house/size/value facet.  If it is set to @i[:setf] then that
facet will also be setf-able; e.g., it would be
possible to modify the @i[value] of the @i[size] slot of a
house instance by saying: @i[(setf (house-size.value house1) <new-size-value>)].

  It is important to note that there are two different ways to refer to
frames in @b[Parmenides].  Classes are always referred to by name, as in
Framekit, and are defined by @b(def-frame).
@i(Instances), on the other hand, should be thought of as abstract
data structures which are created by the make functions (below), and which the
access functions operate on.  However, the make functions allow one
to name an instance.  This gives the user two different ways to refer
to the same instance.

@paragraph(Examples of Def-frame)
@begin(verbatim)
(def-frame animal (cache :*ALL*)
  :consumes yes
  :reproduces (:value 'sexually))
@end(verbatim)
This defines a class named @i(animal), which has no superclasses
and has @i(consumes) and @i(reproduces) slots.
The functions @i(animal-consumes), @i(animal-reproduces),
and @i(animal-reproduces.value) would be defined.  Since
the @i(consumes) slot has no facets, there is no need for an
@i(animal-consumes.value) function.  All of its slots
are cached into its descendants.

The function @b(make-animal) would be defined, and it would take
the keywords @i(:consumes) and @i(:reproduces) as arguments.  To make an instance of animal
with reproduces value NO, we would issue the following command: @*
@i[(setq a1 (make-animal 'a1 :reproduces '(:value NO)))] @*
This makes an animal which doesn't reproduce, calls it a1, and binds
it to the lisp variable a1.  Note that since we didn't specify the
@i[consumes] slot, it receives as default 'yes.

  Note the placement of colons ":" to designate keywords.  Slot
and facet names are stored internally as keywords so that the dependence on which
package the user is in is minimized.
However, @b(def-frame) and the slot and facet retrieval and storage functions take
either keywords or non-keywords as slot and facet
arguments;  they will coerce slot and facet names to keywords if they are not
already.  Thus, it is slightly more efficient to use keywords as arguments, but not
necessary.
Frame names are not stored as keywords.

Every good document needs a transporter frame, so we'll define it as:
@begin(verbatim)
(def-frame transporter (cache (function))
  :function ptrans
  :user (:value 'rider))
@end(verbatim)

This defines a frame class @i(transporter), with slots
@i[function] and @i[user].  The @i[function] slot has no facets - its default value
is @i(ptrans).

Now we can define a more complex frame, horse, in terms of these two frames:
@begin(verbatim)
(def-frame horse (is-a (animal transporter))
  :consumes (:value 'oats :when 'daily))
@end(verbatim)

The horse class will have the slots: @i[consumes, reproduces,] and @i[function].
Note that while the horse class obtains the
@i(reproduces) and @i(function) slots from the animal and transporter classes,
the @i[consumes] slot is locally defined as @i[(:value 'oats :when 'daily)]
instead.
Also, note that the @i[user] slot is
not inherited from transporter since it is not in the @i(cache) list of
transporter.

@End (description)

The slot and facet-access functions are fast because they are pre-compiled.
However, they can only be used if the programmer knows the class, slot and
facet names at code-writing (or compile)
time; otherwise, the more general functions such as
@b(get-facet) and @b(set-facet) (described below) should be used.

  Note that in order to retrieve a value using inheritance or demons,
one can't use the simple slot-access functions defined by Def-frame; rather,
one should use @b(get-facet) or @b(get-facet-demons.)  Perhaps demon-checking
should be compiled into the slot-access functions; however, this would take
much extra space.

@Begin (description)
@p(def-frame* <name> <cplist> <slots>)
  The non-macro form of @b(def-frame) (i.e., evaluates all of its arguments).
Note that @i(<slots>) is not a rest argument in this function.
@end(description)

@subsection(Commands that Make Instances)

  In the
following functions,  where the argument @i(<frame>) occurs,  either
the name of a frame or a frame instance may be given.

@Begin (description)
@p(make-frame <classname> <instance-name> . <slot-values>)
  This is a general function for making instances of the given @i(<classname>).
@i(<instance-name>) is the name of the instance
(optionally nil for no name),  since @b(Parmenides) functions may take
frame names as arguments.
@i(<slot-values>) has the same syntax as @i(<slot-contents>) of @b(def-frame)
except the slot names are keywords.
@b(Make-frame) returns an abstract data structure
which represents
an instance of the frame, and provides the default slot
values that are described by the frame class.  For slots without facets,
any lisp object may be given; for slots with facets, a facet-plist should
be given as the value.  All arguments are evaluated.
For example, to make an instance of an @i(animal) defined by the
previous @b(def-frame) example, we could write:
@begin(verbatim)
(make-frame 'animal 'a1
	:reproduces '(:value usually-not))
@end(verbatim)
This returns a new instance of animal, called a1, with a @i(reproduces)
slot value of @i(usually-not).
Note that since we didn't specify the @i(consumes)
slot, it gets the default value of yes.
Note also that for each class <classname> defined, a make function called @b(make-<classname>)
is defined; for example @b(make-animal) would be defined when the animal class
is defined.
@comment[  A more interesting example might be
one which defines a mule in terms of a horse and donkey.]

@p(make-frame0 <classname> <instance-name> <slot-values>)
Same syntax and definition as @b(make-frame), except @i(<slot-values>)
is a list instead of a rest argument.
@end(description)

@subsection(Data Retrieval Commands)
@Begin (description)
@p(get-facet <frame> <slotname> <facetname>)
This function returns multiple values.  The first value returned is the
filler found in the specified facet of the slot of the given frame.
If the given slot is not
cached in the frame, then it tries to inherit the value from the parents.
It searches the parents in a left-to-right, depth-first fashion.
If either the slot or the facet cannot be found, it returns NIL as the
second value (it also returns NIL for the first value in this case);
if it is found then it returns T as the second value.

It works on both frame classes and instances.  When a class is
given, @i(get-facet) returns the default value for that facet. <frame> may
be either the name of a frame or a frame data structure.

@p(get-value <frame> <slotname>)
Equivalent to: @b[get-facet <frame> <slotname> :value].
For example,
@i[(get-value 'horse :consumes)] would return values 'oats and T.

@p(get-immediate-facet <frame> <slotname> <facetname>)
The same as @b(get-facet) except only retrieves data from the given frame (i.e., doesn't perform
inheritance).  Thus it is faster than @b(get-facet).
Also returns two values.  The second value is nil if the given slot is not
local in the given frame.

@p(get-immediate-value <frame> <slotname>)
Equivalent to: @b[get-immediate-facet <frame> <slotname> :value].

@p(get-value-demons <frame> <slotname>)
Equivalent to: @b[get-facet-demons <frame> <slotname> :value].
See section @ref[demons] for an explanation of demons.

@p(get-slot <frame> <slotname>)
Returns the slot <slotname> for frame <frame>, attempting inheritance
if the slot is not found locally.  Like @b(get-facet), it returns
two values.  The first value returned is the value found in the specified
slot; the second value returned is T iff the specified slot could be found.
The difference between @b(get-slot) and @b(get-value) is that @b(get-value)
gets the value facet of the slot (therefore assuming that the slot has
facets), whereas @b(get-slot) is normally used for slots without facets,
since it returns whatever data structure is stored at the slot level (not the
facet level).

@p(get-immediate-slot <frame> <slotname>)
Like @b(get-slot) except doesn't try inheritance.  Thus it is faster than @b(get-slot).

@p(get-generic-value <frame> <slotname>)
  A cross between @b(get-facet) and @b(get-slot).  If <slotname> does not
have facets in <frame>,
then this is equivalent to @b(get-slot).  If <slotname> has facets, then it
is equivalent to
@b(get-value) (it return the value facet of the slot).

@p(get-cslot <frame> <slotname>)
Returns the value of the class slot <slotname> for frame <frame>.
Sample class slot names are @i(:propagate) and @i(:setable).

@p(get-facet-demons <frame> <slotname> <facetname>)
Like @b(get-facet), except if it gets a NIL cached value, tries running
first, the local if-needed demons, then the if-needed demons attached
to the frame.  Doesn't try inheritance.

  To check that a slot is local, use @b(local-p).

@p(local-p <frame> <slotname>)
Returns T iff the slot named @i(<slotname>) is local in the given frame
(i.e., if the @i[:depth] facet of the slot is equal to 0).  Being local
means that it must have gotten its value from a @b[set-facet], @b[setf],
@b[make-frame] or a set function if it's
an instance, or from a @b[set-facet], @b[setf] or @b[def-frame] if it's a class.
If the slot has no facets then it is defined to be local, although if it
doesn't have a depth facet, then it is defined
to not be local.
To get a feel for the depth facet and localness,
try defining some classes, subclasses and instances, then check the depth of
various slots in them.

@p(pa-class-of <frame>)
Given a frame, returns the name of the class that the frame is an instance of.

@p(isas <class>)
Given the class, returns an ordered list of all the classnames that
@i(<class>) is-a, including superclasses of superclasses.
For example, @i[(isas 'horse)] would return (animal
transporter).  If animal was is-a living-thing, then @i[(isas 'horse)] would
return (animal transporter living-thing).

@p(immediate-isas <class>)
  Like @b(isas), but only returns the immediate ancestors (i.e., the value of
the @i[is-a] c-slot).
For example, if animal was is-a living-thing, then @i[(immediate-isas 'horse)] would
return (animal transporter).

@p(inverse-isas <class>)
Returns a list of class names which are the immediate is-a of the
given @i(<class>). For example,
both (inverse-isas 'animal) and (inverse-isas 'transporter) would return
the singleton (horse).  Note that inverse-isas only returns immediate
inverse-isas, in contrast to the way isas returns @i(all) isas.  This
is because @b(inverse-isas) is used for marker propagation, where it
is desired to have only the immediate ancestors of the given class.
However, it is often useful to know if a given class is is-a another class,
even if indirectly.
This function returns NIL if the argument is not a class, because only
classes have inverse-isas.

@p(instances-of <class>)
Returns a list of the instances of the given frame class, including
the instance representing the class.

@p(set-instances-of <class> <instances>)
Sets the instances of the given class.  This function should be used with
care.

@p(instance-names-of <class>)
Returns a list of the names of the instances of the given frame class.
Doesn't include any instances which don't have names.

@p[With-All-Subinstances-Of <class> <lambda>]
@i[Example: (with-all-subinstances-of 'relation
		#'(lambda (instance) (pp-frame instance)))]

  This macro can be used to perform some function on instances of a class,
as well as the instances of all the classes under the given class in the
hierarchy.  The given <lambda> must be a lambda which takes one argument,
the instance.  This macro runs that lambda on every instance of <class> and
the instances of every class which is-a <class>, including the instance
representing classes.  To filter out instances representing classes, use
@b[classp], below.

@p(get-slot-names <frame>)
Returns a list containing the names of all the local (cached)
slots of the given
frame.  Note that slot names are generally referred to as keywords, and are
always stored internally as keywords.

@p[do-facets (<facet-name> <facet-val> <slot> <frame>) &rest expressions]
This macro
executes the given expressions for each facet in the given <slot> and <frame>.
The name of the facet (which will always be a keyword) is bound to <facet-name>
and the value is bound to <facet-val>.  For example:
@programexample{
(do-facets (name val :consumes 'horse)
  (format T "~S: ~S~%" name val))}

would print the name and value of each facet in the @i[:consumes] slot of
the given @i[horse].

Note that this function only works for faceted slots; also it is much more
efficient than calling @b[get-facet] on each facet.

@end(description)

@subsection(Data Storage Commands)

@begin(description)
@p(set-facet <frame> <slotname> <facetname> <newvalue>)
Sets the facet of the slot of the given frame (class or instance) to the
given @i(<newvalue>).  If it's a class, then the value will be propagated
down to all sub-classes and instances.  Of course set-facet will not
work (and will cause an error) if one tries to set a facet of a slot
which doesn't have facets.

@p(set-value <frame> <slotname> <newvalue>)
Equivalent to: @b[(set-facet <frame> <slotname> :value <newvalue>)].

@p(set-slot <frame> <slotname> <newvalue>)
Sets the value of the slot of the given frame (class or instance) to the
given @i(<newvalue>).  Does not propagate to the instances or sub-classes
or fire demons, since there are no facets on the slot to tell Parmenides
whether it is a local value or what the demons are.

@p(set-cslot <class> <cslotname> <newvalue>)
Sets the value of the class slot of the given frame class to the
given @i(<newvalue>).  Note that unlike @b[set-slot], this function only
works with classes.  Does not propagate to the instances or sub-classes.

@p(set-facet-demons <frame> <slotname> <facetname> <newvalue>)
Same as set-facet, except also fires any pre-if-set and post-if-set
demons it finds attached to the slot or frame class.

@p(set-value-demons <frame> <slotname> <newvalue>)
Equivalent to: @b[(set-facet-demons <frame> <slotname> :value <newvalue>)].

@p(add-to-facet <frame> <slotname> <facetname> <filler>)
Adds the given filler to the list of values stored in the position in
@i(<frame>) specified by @i(<slotname>) and @i(<facetname>).

@p(add-to-value <frame> <slotname> <filler>)
Adds the given filler to the list of values stored in the @i[value] facet
of @i[<slotname>] in
@i(<frame>).

@p(add-to-slot <frame> <slotname> <filler>)
Adds the given filler to the list of values under @i(<slotname>) in
@i(<frame>).

@p(add-to-cslot <class> <cslotname> <filler>)
Adds the given filler to the list of values under class-slot @i(<cslotname>) in
class @i(<class>).  Note that the first argument must be a frame class.

@p(add-to-facet-demons <frame> <slotname> <facetname> <filler>)
Adds the given filler to the list of values stored in the position in
@i(<frame>) specified by @i(<slotname>) and @i(<facetname>).
Since @b(add-to-facet-demons) uses @b(set-facet-demons), it runs the
same demons that would be run by @b(set-facet-demons).

@begin[comment]		This isn't user-friendly yet since it doesn't
accept names as a <frame> arg.
@p[modify-frame <frame> <newslots>]
  Modifies chosen slots in the given <frame>.  As in @b[make-frame],
<newslots> is a list of slot-value pairs.  For example:

@begin(verbatim)
(modify-frame 'animal 'a1
	:reproduces '(value usually-not))
@end(verbatim)
@end[comment]

@end(description)


@subsection[Predicates]

@begin(description)

@p(framep <frame>)
Returns T iff <frame> is a valid frame class or instance.

@p[frame-instance-p <frame>]
  Returns T iff <frame> is an instance of a frame.

@p[classp <frame>]
  Returns T iff <frame> is a frame class.
Note that the default
instance representing classes are considered both classes and instances.

@p[slotp <frame> <slot>]
  Returns T iff <slot> is a slot in <frame>.

@p(facetedp <class> <slot-name>)
  Returns T if the given slot in the given class is faceted, NIL otherwise.

@p(isa-p <classname1> <classname2>)
Returns T iff @i(<classname1>) is a sub-class of @i(<classname2>), i.e.,
@i(<classname1>) is under @i(<classname2>) in the is-a hierarchy,
or if @i(<classname1>) is the same as @i(<classname2>).

@p(isa-instance <frame> <classname>)
Returns T iff the given frame is an instance of the class named
@i(<classname>), or any of its superclasses. @b[isa-instance]
could be defined as:
@verbatim[(member <classname> (isas (pa-class-of <frame>)))]

@p(isa-or-instance <frame> <classname>)
A combination of @b(isa-p) and @b(isa-instance).  Returns T iff @i(<frame>)
is a subclass of @i(<classname>) or is an instance of @i(<classname>) or
any of its superclasses.
@end(description)


@subsection(Miscellaneous Commands)

@begin(description)
@p[frame <framename>]
Given the name of a frame, this returns the actual frame.  Frame classes are
generally referred
to by name, whereas instances can be referred to by either name or actual frame.
When the name of a frame class is given to @b(frame), the default frame instance
is returned.

@p[pp-frame <frame> &key :stream :all-slots]
Pretty-prints the given @i[<frame>], which
may be either an instance or a class.
The keyword @i[:stream] defaults to the standard output (e.g., the
terminal). @i[:all-slots] has a default value of T,
which means that all slots are printed (but see @b[save-frame]).

@p[save-frame <frame> &key :stream :all-slots]
Writes the given @i[<frame>], in Parmenides-readable form,
to the specified stream, or to the standard
output if no stream argument is supplied.
@i[:all-slots] has a default value of NIL which means that
only those slots which differ from the corresponding parent's slots
are printed.

@p(copy-frame <frame>)
Returns a copy of the given frame.  The data structures which are the
values of the facets are not copied.
Note that this function views <frame> as a frame instance, and doesn't copy
any of the class information.  This is because it isn't possible
for two different classes to have the same instances,
since instances are instances of only one class.

@p(remove-frame <frame>)
Removes the given frame (instance or class).
If @i(frame) is an instance, then it will be un-linked from its class
(i.e., the result of @b(instances-of <class-of-frame>) will not include
@i{<frame>}).  It @i(frame) is a class, then the class will be removed
from the list of inverse-isas which each of its parents contain, and each
of its children classes will no longer have it as their parent.  All other
data structures pertaining to that class will be deleted, so that it
will no longer be considered a frame or a class.

@p[define-facet-getter <classname> <slots> <facets>]@*
  @b[Parmenides] usually defines access functions for only the @i[value] facet.
To cause @b[Parmenides] to define access functions for facets other than
@i[value], use this macro.  For example, executing: @i[(define-facet-getter
house :age :depth)] allows one to retrieve the @i[depth] of the @i[age] of
a house quickly by typing: @i[(house-age.depth <house>)].  <slots> and
<facets> may be either lists or atoms.

@p[define-facet-setter <classname> <slots> <facets>]@*
  Analagous to @b[define-facet-getter].  Defines a facet-setting function
of the form: @i[(set-<class>-<slot>.<facet> frame value)].

@p[define-facet-accessors <classname> <slots> <facets>]@*
  Defines both a facet-getting and a facet-setting function for the
specified facet(s).

@p(add-slot <classname> <slot-name> <slot-contents> &key cache)@*
@I[example: (add-slot 'horse :rider '(value 'person))]
Augments the given class by adding the slot <slot-name> with description
@i(<slot-contents>) to the class.
Only works for classes, since instances should never have slots that their classes don't have.
Propagates the change to all instances of the class.
If @i(cache) is true, then adds the @i(<slot-name>) to the list of slots to
cache, and propagates the added slot to all subclasses of @i(<classname>).

@p(add-cslot <classname> <slot-name> <slot-contents>)@*
@i[Example: (add-cslot 'horse :rider 'person)]
Augments the class slot of the given class by adding the slot <slot-name> with description
@i(<slot-contents>) to the class's class-slot data structure.
Only works for classes.
Doesn't propagates the change to instances of the class.

@end[description]

@section(Relations)

  @b[Parmenides] allows users to define their own relations, in order
to allow links between frame instances and classes other than
the standard @i(is-a) and @i(instance) links.
Relations are defined as classes themselves,
through @b(def-frame).  User-defined relations allow one to specify which
slots to inherit through the relation, and how to combine values inherited from other frames.
Relations may also have inverses, in which case certain bookkeeping is automatically
performed by the system.

@subsection(Relation Slots Recognized by the System)

@begin(itemize)

@i(slots-inherited)	Has a value facet since it can be a list of atoms.
@i(Slots-inherited) should
be a list of <slot-name> that are inherited from the parent frame.
An extended syntax also allows the list (<slot-name> <combination-type>) to be
used in the place of a <slot-name> in the list.  This allows individual slots
to
have combination types which aren't the same as the @i(combination-type) for
the relation class.
The keyword :*ALL*
is also recognized for the value of this slot.

@i(combination-type)	Facet-less since its value is an atom.
The @i(combination-type) slot defines how values are combined with
a frame from the parent frame.  Currently its value can be either
@i(:first), @i(:append) or @i(:nconc).  @i(:First) means that the most local
frame's value has precedence; @i(:append) and @i(:nconc) mean to
append or nconc the values
from the related frame to get the result.

@i(has-inverses)	T or NIL (facetless).  If true, then the relation is defined to have inverses.
Inverse relations are supported in a variety of ways in @b(Parmenides).  When a class
which is a relation with inverses is defined,  the class
corresponding to its inverse is also defined.  Also,  when an instance-making function or a
data-storage function such
as @b(set-slot) or @b(set-facet) stores a value on a slot @i(<relation-slot>) which is a relation with
inverses, the value @i(<range>) is assumed to be a frame.  If it is, then @i(<range>) should have
a slot @i(<inverse-relation-slot>) whose name is the same
as the inverse relation corresponding to @i(<relation-slot>).
The @i(<range>) frame will automatically have the original frame added to the list
of values stored in its @i(<inverse-relation-slot>).
When a class is defined to have a relation slot, and the value of that slot is another class (<range>),
then the inverse relation slot corresponding to that slot will be added to the <range> frame, and
the value will be the original frame.
The examples below will make this more concrete.

@i(inverse-names)	Symbol (facetless).  The name to be given to the inverse relation if the
relation has inverses.  Ignored if the relation does not have inverses.  This is the name used to
make the inverse relation class when a relation class is defined, and is also the name of the
slot which is updated when a frame is stored in a relation slot.  If not supplied, then the
inverse name is constructed by concatenating "inverse-" to the beginning of the relation name.

@end(itemize)

@subsection(Examples of Relations)

For example, suppose the @i(part-of) relation were defined as follows:
@begin(programexample)
  (def-frame part-of (:is-a relation :propagate nil)
    :combination-type :FIRST
    :slots-inherited (:value '(location made-from))
    :has-inverses T
    :inverse-name contains)
@end(programexample)

Since @i(part-of) is defined to have inverses in this example,
the system would automatically define the @i(contains) relation (if it hadn't already been defined)
as:
@begin(programexample)
  (def-frame contains (:is-a relation :propagate nil)
    :has-inverses T
    :inverse-name part-of)
@end(programexample)

In order to define a frame which is @i(part-of) another, use @i(part-of)
in the class slot plist.  For example, to make @i(leg) a part-of @i(table),
first define the table frame, then type:
@begin(programexample)
(def-frame leg (part-of table)
  :function (:value 'support))
@end(programexample)

Since the @i(part-of) relation was defined to inherit the slots @i(location) and @i(made-from),
the @i(leg) frame will receive these slots from @i(table).  Also, since @i(part-of) has inverses,
and that inverse is named @i(contains), the @i(table) frame will have @i(leg) added to its @i(contains)
class slot. (If it didn't already have a contains class slot, then that slot will be added).

Note that the class(es) that a frame is a relation to need not be a list if it is only in relation
to one frame.

Instances may also have relations with other instances or classes.
Since slots can not be added to instances, instances may inherit only values from other frames.
The relation is specified as an instance slot when defining the class.
For example, in order to make different instances of legs
part-of different instances of tables,  the
leg frame would be defined thusly:

@begin(programexample)
(def-frame leg (part-of table)
  :part-of table
  :function (:value 'sit-on))
@end(programexample)

  This would cause @b(Parmenides) to add the @i(contains) instance slot to the @i(table) frame if it didn't
already exist.  @i(Leg) would be added to the list of values of the @i(contains) slot of @i(table).

To inherit values from table frames,
when a leg instance is made, a table is given as the value of the
@i(part-of) slot:
@begin(programexample)
(make-leg 'leg1
    :part-of 'table1)
@end(programexample)

@i(leg1) would be added to the list of values in the @i(contains) slot of @i(table1).

Currently the @i(is-a) relation is special, so
if the user defines the is-a relation, it will not affect
inheritance through @i(is-a) specified as a class slot.

@section(Demons)
@label[demons]

  Instead of having only one type of
demon associated with access functions, @b(Parmenides) provides two different
types.  Sometimes it is useful to perform some action before a slot is
set (for example, to delete the old slot value from some data base), and then
perform another action afterwards.  Similarly, sometimes it is useful to
perform some action both before a retrieve request (e.g., to compute the
value), and after the request (e.g., to update a data base).

Demons (corresponding to methods in object-based languages)
are single s-expressions which are evaluated when
certain facets are accessed (set or retrieved).
Since demons can be stored as both class slots and slot facets,
they can be associated
with either a class or a slot, or both.  When a demon is evaluated,
the following lisp variables are locally set (although @i(specially) bound -
see p. 157 of @b(Common Lisp, The Language), by Guy L. Steele, Jr.):
@begin(itemize)
framename -- the name of the frame that is being accessed

slotname -- the name of the slot being accessed

facetname  -- the name of the facet being accessed

frame -- the actual frame data structure (not the name) being accessed

snum -- slot number of the slot

facetnum -- facet number

newval -- value being stored (only for demons which set values)
@end(itemize)

The following demons types
are recognized by @b(Parmenides).

@begin(itemize)

@b(if-needed).  When @b(get-facet-demons) is used to retrieve a facet,
the following algorithm is used:
@begin(enumerate)
Try to get an immediate value.

If there is no immediate value (i.e., if the value stored there is NIL),
then try to invoke the if-needed demon stored on that slot.

If there is no demon on that slot, then try calling a demon stored on
the class.
@end(enumerate)

@b(pre-if-set).
When @b(set-facet-demons) is used to set the value of a facet, before
the frame is changed, any pre-if-set demons stored on either the slot
or the class will be evaluated.

@b(post-if-set).
When @b(set-facet-demons) is used to set the value of a facet, after
the frame is changed, any post-if-set demons stored on either the slot
or the class will be evaluated.

@end(itemize)

In summary, when the @b(set-facet-demons) command is issued, the following
sequence occurs:
@begin(enumerate)
Execute any @b(pre-if-set) demons associated with the slot.

Execute any @b(pre-if-set) demons associated with the class.

Set the facet to the new value.

Execute any @b(post-if-set) demons associated with the slot.

Execute any @b(post-if-set) demons associated with the class.
@end(enumerate)

@section[Multiple Language Messages]
@label[multi-language]

  Parmenides can produce its messages in a number of languages.  Although
it currently supports only English and Spanish, it is easy to add new
languages through message files.  When Parmenides is loaded,
it loads a message file for the appropriate language.  When installing
Parmenides, the definition of the Parmenides directory must be set - change
the variable *PA-PATHNAME*, near the top of parmenides.lisp.  The language
can also be set by changing the variable *LANGUAGE*.  The current language
can be changed by calling @b[define-language] with the name of the
appropriate language.  "eng" defines English, and "esp" defines Spanish.

@section(Compiling User Frame Files)

As mentioned previously,
user frame files (i.e., files containing Def-frame) may be compiled when
a set of frames is known to be stable.  After that, the compiled frame
file may
be loaded in without having to load in the source frame file.  (compiled
file extensions for spice lisp are .fasl; for the Symbolics they are .bin).
This results
in considerable loading and executing speed up, since in the act of compiling
the files, the Def-frame command is executed and all the access and make
functions are defined.  However, the Parmenides program
should be loaded in before the frame files are either compiled or
loaded, in order to get the definitions of the general
Parmenides functions (such as get-facet).

@section(Future enhancements)

@begin(itemize)

Frame classes should be able to be declared to not have instances if they
are only used for concepts.  For example, one probably wouldn't make any
instances of the animal class if one had sub-classes such as mammal and
amphibian.  This would save the time and storage space of creating a
default instance and defining maker and access functions.

Frames should be able to be both classes and instances.  For example,
the person class is an instance of the class class.

In order to make class slots more complete, they should also be allowed
on slots.  If a slot had a meta-value, this would only be stored in the
slots of the class, and not the instances.

Ability to delete facets and slots.

@end(itemize)

@section(Acquiring, Loading, Compiling and Using Parmenides)

@comment{
To run Parmenides under Spice lisp on a Perq,  only one file
need be loaded from the cmu-ri-ml vax:
/ml/usr/pshell/parmenides/parmenides.sfasl. }
  @b[Parmenides] is free to affiliates and members of the CMU community.  It
is available for a nominal fee to all others.  To request a copy,
send mail to pshell@@ml.ri.cmu.edu or write to:
@begin(format)

	Peter Shell
	School of Computer Science
	Carnegie Mellon University
	Pittsburgh, PA  15213
@end(format)

The Parmenides source file is /usr/pshell/parmenides/parmenides.lisp
on the ML vax or Wiener RT,
and works under any reasonable dialect of Common Lisp.
Parmenides should be compiled before used.  Parmenides
must be loaded before it is compiled.
In order to load the multi-lingual message files, the variable
*PA-PATHNAME*,  near the top of parmenides.lisp, must be defined.  This
variable defines the directory in which the Parmenides files are found.
To run Parmenides under CMU Common lisp
on an RT, load /../wiener/usr/pshell/parmenides/parmenides.fasl.
Parmenides is put into a package called 'Parmenides' with nicknames 'Parm'
and 'Pa'.  After loading Parmenides, one should do: @i[(use-package
'parmenides)].

  This document is /../ml/usr/pshell/parmenides/parmenides.{mss, doc, ps}.
This directory also contains a test file called prtest.lisp.  After
compiling Parmenides, load this file to ensure that Parmenides works okay
on your system.
If there are any questions or comments,
please send mail to PShell@@ml.ri.cmu.edu.

@begin(comment)
@newpage
@appendix(Timings)

The speed of @b(Parmenides) will be compared to @b(Framekit) and @b(Defstruct).
Defstruct is a Commonlisp structures package.  Structures are like very
simple frames instances: they don't have facets, class hierarchies, or demons.
The timings were done on a Perq-II under Spice lisp with 2 megabytes of
memory.
The functions were timed by running them 100 times each, and the average
taken over 3 of these 100 iterations.
All timings are with compiled code unless otherwise noted.
(Jiffies are 1/60 second units).
The Commonlisp @b(time) function was used.  Timings seems to widely vary
over different amounts of free memory on the Perq.  However, each
of these timings were done with roughly the same amount of free
space.

@begin(verbatim)

@b[Framekit] get-facet (first slot and facet):
33.67 jiffies

@b[Parmenides] get-facet (first slot and facet):

17.67 jiffies

@b[Defstruct] slot access function (any slot).

7.33 jiffies

@b[Parmenides] slot/facet access function (facet #1 (usually value), any slot).

9.0 jiffies

@b[Parmenides] slot-facet access function (not facet #1, so not pre-compiled).

28.0 jiffies

@end(verbatim)

@end(comment)
