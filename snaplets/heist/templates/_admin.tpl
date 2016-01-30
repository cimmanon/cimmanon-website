<apply template="_layout">
	<h1>Administration</h1>

	<div class="admin">
		<nav>
			<ul>
				<li><a href="/admin/projects/">Projects</a>
					<ul>
						<isProject><li><name>Project Name</name>
							<ul>
								<li><a href="/projects/${slug}/">View</a></li>
								<li><a href="/admin/projects/${slug}/">Edit</a></li>
								<li><a href="/admin/projects/${slug}/components/">Components</a>
									<ul>
										<isComponent><li><type>Component</type> (<date>2016-01-10</date>)
											<ul>
												<li><a href="/admin/projects/${slug}/components/${type}/${date}/">Edit</a></li>
												<li><a href="/admin/projects/${slug}/components/${type}/${date}/images">Images</a></li>
											</ul></li></isComponent>
										<li><a href="/admin/projects/${slug}/components/Design/">Add Design</a></li>
										<li><a href="/admin/projects/${slug}/components/Development/">Add Development</a></li>
										<li><a href="/admin/projects/${slug}/components/Flash/">Add Flash</a></li>
									</ul></li>
							</ul></li></isProject>
						<li><a href="/admin/projects/add">Add Project</a></li>
						<li><a href="/admin/reload">Reload Templates</a></li>
					</ul></li>
				<li>Components</li>
				<li>Tags</li>
			</ul>
		</nav>

		<article>
			<apply-content />
		</article>
	</div>
</apply>