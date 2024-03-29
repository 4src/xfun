<img src="https://img.shields.io/badge/tests-passing-green"> <img
src="https://img.shields.io/badge/sbcl-2.3-orange"> <img 
src="https://img.shields.io/badge/purpose-se--ai-pink"> <img 
src="https://img.shields.io/badge/platform-osx,linux-9cf"> by
<a href="mailto:timm@ieee.org">timm@ieee.org</a>
<img align=right width=300 src="/etc/img/dots4.png">
<h1>SE and AI, just the important bits</h1>
<p>I've been working on AI and SE for oer 30 years.
Sometimes, that tech is inherently complex and hard to understand.
And often it isn't. Here I present the least code needed to
understand  the most about SE and AI that can explain itself.
  </p><br clear=all>

## What is AI? What is SE?

<img src="/etc/img/phases.png" align=right width=400>

Here's something that moght surprise you.
Most of the time, SE is not  about programming:

- _Before_ coding, _choices_ must be made about what to code. 
_ _After_ coding comes testing and integration to other code, and maintenance. - And all the while, the people called "programmers" do many things besides
coding like  take meetings, support users,   working on their career skills, etc etc

Depending on what study you read,
programmers spend about 16 to 35% of their time actually coding (brookes, ) of their time coding and the rest doing other things.
The  figure  at right (from a 2023 paper by  Long et al.[^long23]) shows the amount of time spent coding
during six activities (listed below). Note that even while "coding", developers usually code less than half the time. 

1. requirements analysis;
2. architecture & design; 
3. coding;
4. system integration (combiming your code with everyone else's);
5. qualification testing;
6. development test & evaluation.

[^long23]: D. Long, S. Drylie, J. Ritschel and C. Koschnick, "An Assessment of Rules of Thumb for Software Phase Management, and the Relationship between Phase Effort and Schedule Success," in IEEE Transactions on Software Engineering, doi: 10.1109/TSE.2023.3339383.

expertise is not about one solution, its gfeneralizing acorss N solutions. can you glance at a solution and propoose 5 alternatives? can be cluster them u into one most anstract solution that describes what is true across all the colsutions? can you comment on the trade-offs
between these solutions?

## The Great Secret to Simplicity

Two of my favorite researchers, Abram Hindle and Prem Devanbu [^hindle16], 
offer the great secret to simplifying softwre develpment:

- Programming languages, in theory, are complex, flexible and powerful, 
but the programs that real people
actually write are mostly simple and rather repetitive,
and thus they have usefully predictable statistical properties that can be captured ...
and leveraged for software engineering tasks

[^hindle16]: Hindle, A., Barr, E. T., Gabel, M., Su, Z., & Devanbu, P. (2016).
On the naturalness of software. Communications of the ACM, 59(5), 122-131.

Now programs are tools for changing the world; i.e. they are functions $f$ for 
turning inputs  $x$ to outputs $y$. This means
if software functions $f$ have simple and  repetitive properties, 
then those same properties  cause, or are caused by, simple and  repetitive properties in the inputs and outputs.

Now here's where the AI comes in. Picture a table of data, where each row is one run of a system. This
table has one column for each output $y_i$ and input $x_i$.  When table rows     contains repetitive structures, then lots
of rows can be reduced to a smaller number [^ssl]. So one tactic for simplification is this: don't look at all the details.
Cluster what your know into a few groups, then just reason about a few items per groups [^bless]. This can be very usefuk for many applications:

[^bless]: I'm not alone in saying that   complex things are inherently simplifiable, has been said by many people. XXX narrows, PCA, backdoors, etcetc, bless of dimensionality. But in all that, I have not seen work that reduces things as much as my code.
 
- **Test case reduction:** Instead of testing everything,
  just test a small sample [^ling23].
- **Software configuration:** Instead of trying all configurations
  (which can be impossibly slow), use the clustering to  a sampling just a representative subset  [^nair18a].
- **Requirements engineering:** Trading off between multiple stakeholders.
  means exploring decision decisions,
  looking for some point that is not too unacceptable. But if we explore too much, and ask too many questions,
  then  stakeholders grow weary and unrelaible.  Clusters can be used as a _surrogate_ which we query, a lot, in place of humans [^nair18b].
- **Software analytics:** Data analytics  uses data miners, and data miners are
  notoriously "brittle"; i.e. what they do  can change dramatically
  if we tweak a miner's control parameters. Trying all the possible tunings
  is impractical but, using clustering, we can check a wide range of possibilities [^nair18b].

[^nair18b]: Nair, V., Yu, Z., Menzies, T., Siegmund, N., & Apel, S. (2018). Finding faster configurations using FLASH.
IEEE Transactions on Software Engineering, 46(7), 794-811.

[^ling23]: Ling, X., & Menzies, T. (2023). On the Benefits of Semi-Supervised Test Case Generation for Cyber-Physical Systems. arXiv preprint arXiv:2305.03714.

[^nair18a]: Nair, V., Menzies, T., Siegmund, N., & Apel, S. (2018). Faster discovery of faster system configurations with spectral learning. Automated Software Engineering, 25, 247-277.

[^ssl]: There's actually a mathematical proof of this, which we won't use. But for the record,
the Johnson–Lindenstrauss lemma [^john86] states that a set of points in a high-dimensional space can be embedded into a space of much lower dimension in such a way that distances between the points are nearly preserved. 

[^john86]: Johnson, W.B., Lindenstrauss, J. & Schechtman, G. Extensions of lipschitz maps into Banach spaces. Israel J. Math. 54, 129–138 (1986). https://doi.org/10.1007/BF02764938

```lisp <less cli>
(defun cli (options &aux it)
  "CLI items that match `flag` can update items in `*settings`. For non-boolean
   settings, we expect a value after a flag. For boolean settings, flags do not
   need value (we just flip toe old value)."
  (loop :for (key flag help b4) :in options 
        :collect (list key flag help
                       (if (setf it (member flag (args) :test #'string=))
                           (cond ((eq b4 t) nil)
                                 ((eq b4 nil) t)
                                 (t (str2thing (second it))))
                           b4))))
```

