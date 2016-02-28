<bind tag="pageTitle">R&eacute;sum&eacute;</bind>
<bind tag="bodyId">resume</bind>
<apply template="_layout">
	<h1 data-name="Christina A. Siepker"><pageTitle /></h1>

	<blockquote>
		<p>I walk the line between designer and developer.  Some days I feel like a designer trying to muddle my way through code, other days I think I'm a developer struggling to make something look nice.</p>
	</blockquote>

	<section id="skills" class="major">
		<h1>Skills</h1>

		<div class="listing">
			<article>
				<h1>Design</h1>

				<p>I got into web design in 1998 so that I could help out with my game clan's website.  Within a few months, I was writing markup by hand and using CSS to style it.  My first table-less design was in 2002, and my last active table-based design was converted to using CSS in 2004.  I started exploring responsive web design in 2012 for work.</p>

				<p>My style has evolved quite a bit throughout the years.  A lot of my early designs feature digital paintings that frame the site's content, now I let the content drive the design.  The one thing that hasn't change is my commitment to progressive enhancement.  The user should never think they're looking at a broken design, but users with a modern browser will receive the best experience.</p>

				<ul class="skills">
					<li data-level="10">HTML</li>
					<li data-level="10">CSS</li>
					<li data-level="10">Sass</li>
					<li data-level="8">Responsive Web Design</li>
					<li data-level="6">User Experience</li>
					<li data-level="5">Photoshop</li>
					<li data-level="4">Illustrator</li>
					<li data-level="4">Inkscape</li>
				</ul>
			</article>

			<article>
				<h1>Programming</h1>

				<p>Programming became a part of my skill set in 2002 when I needed to write small programs and web applications.  I picked up a bit of Scala in 2012, but quickly moved on to Haskell.  Writing code involves a lot more trial and error and Google-fu than my other skills.  When it comes to solving complex programming problems, I find that it helps to draw them out on paper.</p>

				<ul class="skills">
					<li data-level="5">Haskell</li>
					<li data-level="2">Scala</li>
					<li data-level="2">Pike</li>
					<li data-level="4">PHP</li>
					<li data-level="3">JavaScript</li>
				</ul>
			</article>

			<article>
				<h1>Databases</h1>

				<p>Working with databases is a relatively new discipline for me.  I was introduced to the world of databases in 2002 with MySQL for managing my online diary, but I didn't take it seriously until 2005 when I started working on Gamependium.  I began exploring triggers and query optimization in 2012 when I started working with millions of rows of data.</p>

				<ul class="skills">
					<li data-level="7">Data Modeling</li>
					<li data-level="6">PostgreSQL</li>
					<li data-level="5">PLpgSQL</li>
					<li data-level="4">MySQL</li>
					<li data-level="5">Query Optimization</li>
				</ul>
			</article>
		</div>
	</section>

	<section id="experience" class="major">
		<h1>Work Experience</h1>

		<div class="listing">
			<article>
				<header>
				<h1>Data Modeler/Haskell Developer, Zalora</h1>
					<time>March 2014</time> to <time>February 2015</time>
				</header>

				<p>My primary project was to redesign their MySQL database and convert it to PostgreSQL.  Of the ~400 tables that comprised their database, ~200 of them were responsible for storing product data.  I devised a schema that would use ~30 tables to manage the same amount of data while still being normalized.  The simpler schema meant a simpler code base and would provide a better experience for the user.  This project was canceled while I was in the process of writing the conversion.</p>
			</article>

			<article>
				<header>
					<h1>Data Modeler/Web Developer, snapd Inc.</h1>
					<time>November 2011</time> to <time>March 2014</time>
				</header>

				<p>My first project was a web application to manage the planning of flyers for a grocery store.  We used as an experiment on how we were going to rewrite a similar application that runs snapd.  The first version was written in PHP using the Zend framework, but I found it to be difficult to maintain in the long run.  After exploring frameworks in both Scala and Haskell, I settled on Haskell's Snap framework.  Over the course of 2 months, the application was completely rewritten and included a number of improvements over the original.</p>
			</article>
		</div>
	</section>

	<section id="opensource" class="major">
		<h1>Open Source Contributions</h1>

		<div class="listing">
			<article>
				<header>
					<h1>Digestive Functors (Haskell)</h1>
					<address><a href="https://github.com/jaspervdj/digestive-functors">GitHub</a></address>
				</header>

				<p>I added a new formlet that would accept multiple file uploads on a single input element when using the <code>multiple</code> attribute.  I also fixed a minor validation error in the Heist package.</p>
			</article>

			<article>
				<header>
					<h1>Digestive Functors Scotty (Haskell)</h1>
					<address><a href="https://github.com/mmartin/digestive-functors-scotty">GitHub</a></address>
				</header>

				<p>I made a minor adjustment to make the library compatible with Scotty 0.7 and Digestive Functors 0.7.  I needed to use the newer versions of those libraries for a project I was working on at Zalora.</p>
			</article>

			<article>
				<header>
					<h1>Sass (Ruby)</h1>
					<address><a href="https://github.com/sass/sass">GitHub</a></address>
				</header>

				<p>My primary contributions here are in the form of support on the issue tracker.  I assist in helping triage new issues, from pointing out duplicates to resolving user errors.</p>
			</article>

			<article>
				<header>
					<h1>Compass (Ruby, Sass)</h1>
					<address><a href="https://github.com/Compass/compass/">GitHub</a></address>
				</header>

				<p>I rewrote their collection of Flexbox mixins for the 0.12 version.  The original mixins were for the 2009 Flexbox specification, but a few browsers had been supporting the standard specification from 2012 for quite a while when I made the pull request.</p>
			</article>
		</div>
	</section>
</apply>