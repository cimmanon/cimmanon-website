<apply template="_layout">
	<h1>Administration</h1>

	<div class="admin">
		<nav>
			<isComponent><section>
				<h1><type>Component</type>, <date>2016-01-10</date></h1>

				<ul>
					<li><a href="/admin/projects/${slug}/components/${type}/${date}/">Edit</a></li>
					<li><a href="/admin/projects/${slug}/components/${type}/${date}/images">Images</a></li>
				</ul>
			</section></isComponent>

			<isProject><section>
				<h1><name>Project Name</name></h1>

				<ul>
					<li><a href="/projects/${slug}/">View</a></li>
					<li><a href="/admin/projects/${slug}/">Edit</a></li>
					<li><a href="/admin/projects/${slug}/components/">Components</a></li>
					<type><li><a href="/admin/projects/${slug}/components/${name}/">Add <name>Type</name></a></li></type>
				</ul>
			</section></isProject>

			<section>
				<h1>General</h1>

				<ul>
					<li><a href="/admin/projects/">Projects</a></li>
					<li><a href="/admin/projects/add">Add Project</a></li>
					<li>Components</li>
					<li>Tags</li>
					<li><a href="/admin/reload">Reload Templates</a></li>
				</ul>
			</section>
		</nav>

		<article>
			<apply-content />
		</article>
	</div>
</apply>