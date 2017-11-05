<apply template="_layout">
	<h1>Administration</h1>

	<section class="admin">
		<nav>
			<ol>
				<li><span>Configuration</span>
					<ul>
						<li><a href="/admin/projects/">Projects</a></li>
						<li><a href="/admin/projects/add">Add Project</a></li>
						<li><span>Settings</span>
							<ul>
								<li><a href="/admin/settings/component-types">Component Types</a></li>
								<li>Tag Categories</li>
								<li>Tags</li>
							</ul>
						</li>
						<li><a href="/admin/reload">Reload Templates</a></li>
					</ul></li>
				<isProject><li><span><name>Project Name</name></span>
					<ul>
						<li><a href="/projects/${slug}/">View</a></li>
						<li><a href="/admin/projects/${slug}/">Edit</a></li>
						<li><a href="/admin/projects/${slug}/components/">Components</a>
						<li><a href="/admin/projects/${slug}/components/add">Add Component</a></li>
					</ul>
				</li></isProject>
				<isComponent><li><span><type>Component</type>, <date>2016-01-10</date></span>
					<ul>
						<li><a href="/admin/projects/${slug}/components/${type}/${date}/">Edit</a></li>
						<li><a href="/admin/projects/${slug}/components/${type}/${date}/images">Images</a></li>
					</ul>
				</li></isComponent>
			</ol>
		</nav>

		<article>
			<apply-content />
		</article>
	</section>
</apply>
