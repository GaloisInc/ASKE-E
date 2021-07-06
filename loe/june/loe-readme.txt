The LOE tools are experimental; they attempt to quantify a Level of Effort
necessary to manually transform one representation of a model to another.

Please read the "Potential Approaches" section, below, for a discussion of
what we're attempting to accomplish and how that might work.

loe-metrics.py [-t|--tokens] FILE1 FILE2
	Prints information regarding the LOE to transform FILE1 to FILE2.
	This program reports a variety of metrics. Please see the program
	text and the "Notes and Caveats" section, below, for further details.

loe-driver.sh [-t|--tokens] FILE_OR_DIRECTORY FILE_OR_DIRECTORY...
	Prints information regarding the LOE for a sequence of derivations
	of the same model.

Notes and Caveats
------------------

1) It'd be nice if the scalar LOE metric satisfies the triangle inequality.
2) When a directory is specified as input to loe-driver.sh, all files found
   in that directory and its children are catenated before passing to the
   metrics calculation.
3) All input files must contain UTF-8 text.

Potential Approaches
--------------------

= Intent

We wish to find a way to express a "Level of Effort" (LOE) required to
express a model in various representations: descriptive, informal,
formal and executable.

= Challenges

Source documents may include journal articles, declarative models in
multiple forms, and program source code used to reify a model.

= Properties

A LOE metric should satisfy certain properties. Here, E(a) is the value
of the LOE metric for source document "a". We assume that E(a)<E(b) means
that the effort to create the model represented by document "a" is less
than the effort to create the model represented by document "b".

	E(a)=E(b) ^ E(b)=E(a)
	E(a)<E(b) ^ E(b)>E(a)

We assume that a model is reified in a series of one or more derivations
starting with an original source document. Each derivation may be done
manually, done with machine assistance or with a fully automated translator.

The LOE is not the effort required to produce a given derivation using
tooling, but rather the expected human effort required to generate a
derived model without machine assistance. For example, suppose that we have
a formal model and a tool that is able to translate that model into executable
code. The LOE does not represent time or other resources consumed by the
tool, but rather the anticipated effort required for a human to manually
create the executable code from the formal model.

To a (very poor) first approximation, LOE can be represented by the length
of a source text. Such a metric is dependent, of course, not only upon the
source syntax but also upon the experience and coding/writing style of the
document's creator. Still, it's probably fair to say that a longer source
text will require more effort to create (manually) than a shorter text.

Complexity is another metric we could try as a standin for LOE. Without
being aware of the model's representation language (which is our starting
point, considering the variety of possible model representations),
document entropy (compressibility) can represent the "essential" complexity
of a text. For example, ten pages of "0" followed by a "1" is highly
compressible while a page of gibberish is probably not. Intuitively, the
longer the source document and the more distinctly different symbols it
contains, the higher its entropy. Actually compressing the document as a
proxy for its entropy will be heavily biased on short documents (which is
the expected case for models important to ASKE-E) due to the variable length
of metadata and dictionaries embedded by the compression program; we must
therefore calculate document entropy based upon the document's token
frequencies.

= Derivations

Regardless of how many derivation steps or transforms may be performed
mechanically in order to reify a model, we are interested *only* in the
LOE expended by a human in order to reify a source document into an
executable model in one step by a human.

For example, assume we have tooling to transform an ODE represented in
LaTeX to the code necessary to simulate that model in C++. We assume that
the C++ code relies upon already-written libraries; the libraries should
not factor into the LOE calculation. The LaTeX code is probably a line or
two per equation for a small number of equations; the corresponding C++
code might run to several pages. The C++ code is both longer and (by virtue
of being written in a general-purpose programming language rather than a
declarative domain-specific syntax) more complex; we expect that the C++
code will be more compressible than the LaTeX system of equations.

= Triangle Inequality

In light of the need to compare LOE only for a single-step derivation, we
do not require that the LOE metric satifies the triangle inequality. If
LOE(s,t) represents the effort to manually transform source model "s" into
target model "t", then given an original source model "a" and two target
representations "b" and "c", we'd like to know the relationship

	LOE(a,b) R LOE(a,c) .

If "a", for example, is a model presented in a journal paper, "b" is a
DSL representation of "a" and "c" is the bespoke executable program (written
in a general-purpose programming language) reifying "a", then we'd like
to know that LOE(a,b) < LOE(a,c).

Note that the above comparison need not satisfy the triangle inequality.
In other words, it is not important to us that

	LOE(a,b)+LOE(b,c) >= LOE(a,c) .

= Other metrics

Kolmogorov Complexity
https://en.wikipedia.org/wiki/Kolmogorov_complexity

The Kolmogorov Complexity metric attempts to quantify complexity as the
length of the shortest program (in the Turing sense) needed to recreate
the input document. Unfortunately, this is not a computable function.
Compressibility (*including* the requisite dictionary) is an approximate
standin for the Kolmogorov Complexity metric.

Halstead Complexity
https://en.wikipedia.org/wiki/Halstead_complexity_measures

The Halstead Complexity metric is based upon a text's count and relative
frequencies of operators and operands in a program. This clearly won't be
applicable to non-executable documents such as a research paper (the
essential text of the model would need to be identified and extracted)
Furthermore, categorizing document tokens into operators and operands
requires an awareness of the model language.

It may be interesting to test whether Halstead Complexity could be adapted
in a manner that does not require full language-awareness, but rather just
specifying the language in which the measured model is represented, by using
an approximation of the proportion of operands to operators in programs
written in that language.

Embeddings

The Gensim suite of NLP tools provides embeddings other than the LSI/LSA
used in the first draft of our tooling to estimate LOE.




NOTES for late-June edit

https://en.wikipedia.org/wiki/Algorithmic_information_theory
https://en.wikipedia.org/wiki/Minimum_description_length
https://en.wikipedia.org/wiki/Information-based_complexity
https://en.wikipedia.org/wiki/Solomonoff%27s_theory_of_inductive_inference
https://en.wikipedia.org/wiki/Simplicity_theory
