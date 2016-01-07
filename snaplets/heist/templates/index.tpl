<apply template="_layout">
	<h1>Portfolio</h1>

	<blockquote>
		<p>I walk the line between designer and developer.  Some days I feel like a designer trying to muddle my way through code, other days I think I'm a developer struggling to make something look nice.</p>
	</blockquote>

	<project><article class="project">
		<h1><name>Project Name</name></h1>

		<div class="details">
			<url><address><a href="${href}"><href /></a></address></url>

			<p><description></description></p>
		</div>

		<div class="listing">
			<component><section>
				<apply template="components/_component" />
			</section></component>
		</div>

		<aside>
			<image><img src="/images/screenshots/${slug}/${filename}" width="${width}" height="${height}" alt="" /></image>

			<p><a href="/projects/${slug}/" class="more">Project history</a></p>
		</aside>
	</article></project>
</apply>