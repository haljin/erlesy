var width = 1920,
    height = 1024;

var color = d3.scale.category20();

var sizeScale = d3.scale.linear().domain([1, 20]).range([40, 100]).clamp(true);
var force = d3.layout.force()
    .charge(-7500)
    .linkDistance(250)
    .size([width, height]);

var x = d3.scale.ordinal()
    .rangeRoundBands([0, width], .1, .3);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);


function wrap(text, width) {
    text.each(function() {
        var text = d3.select(this),
            words = text.text().split("|").reverse(),
            word,
            line = [],
            lineNumber = 0,
            lineHeight = 1.1, // ems
            y = text.attr("y"),
            dy = parseFloat(text.attr("dy")),
            tspan = text.text(null).append("tspan").attr("x", 0).attr("y", y).attr("dy", dy + "em");
        while (word = words.pop()) {
            line.push(word);
            tspan.text(line.join(" "));
            if (tspan.node().getComputedTextLength() > width) {
                line.pop();
                tspan.text(line.join(" "));
                line = [word];
                tspan = text.append("tspan").attr("x", 0).attr("y", y).attr("dy", ++lineNumber * lineHeight + dy + "em").text(word);
            }
        }
    });
}

d3.json("example_fsm.json", function(error, graph) {
    if (error) throw error;
    var edges = [];
    find_vertex = function(name, vertices, isSource = false) {
        for (i = 0; i < vertices.length; i++)
            if (vertices[i].name == name) {
                if (isSource)
                    vertices[i].outDegree = vertices[i].outDegree ? vertices[i].outDegree + 1 : 1;
                return i;
            }
        return -1;
    };

    collapse_edge = function(from, to, label, edges) {
        for (i = 0; i < edges.length; i++) {
            if (edges[i].source == from && edges[i].target == to) {
                edges[i].label += (" | " + label);
                return;
            }

        }
        edges.push({
            source: from,
            target: to,
            label: label
        });
        return;
    }

    graph.edges.forEach(function(e) {
        var from = find_vertex(e.source,
            graph.vertices, true);
        var to = find_vertex(e.target,
            graph.vertices);
        if (from == to)
            collapse_edge(from, to, e.label, edges);
        else
            edges.push({
                source: from,
                target: to,
                label: e.label
            });
    });

    force
        .nodes(graph.vertices)
        .links(edges)
        .start();

    var linkGroups = svg.selectAll(".linkGroup")
        .data(edges)
        .enter().append("g")
        .attr("class", "linkGroup");

    linkGroups.append("path")
        .attr("class", "link")
        .style("stroke-width", 2);

    linkGroups.append("text")
	.attr("class", "eventText")
        .text(function(d) {
            return d.label;
        });

    var groups = svg.selectAll(".nodeGroup")
        .data(graph.vertices)
        .enter().append("g")
        .attr("class", "nodeGroup")
        .attr("id", function(d) {
            return d.name;
        })
        .call(force.drag);

    groups.append("circle")
        .attr("class", "node")
        .attr("r", function(d) {
            if (!d.outDegree) return 30;
            return sizeScale(d.outDegree);
        })
        .attr("cx", 0)
        .attr("cy", 0)
        .style("fill", color(1))
        .style("visibility", function(d) {
            if (d.name == "init" || d.name == "terminate")
                return "hidden";
            else
                return "visible"
        });

    svg.select("#init")
        .append("circle")
        .attr("class", "node")
        .attr("r", 20)
        .attr("cx", 0)
        .attr("cy", 0)
        .style("fill", "black")

    svg.select("#terminate")
        .append("circle")
        .attr("class", "node")
        .attr("r", 25)
        .attr("cx", 0)
        .attr("cy", 0)
        .style("stroke", "black")
        .style("stroke-width", "1px")
        .style("fill", "none")

    svg.select("#terminate")
        .append("circle")
        .attr("class", "node")
        .attr("r", 20)
        .attr("cx", 0)
        .attr("cy", 0)
        .style("fill", "black")

    groups.append("text")
        .text(function(d) {
            if (d.name != "init" && d.name != "terminate")
                return d.name;
            else
                return "";
        })
        .attr("x", 0)
        .attr("y", 0);

    force.on("tick", function() {
        linkGroups.selectAll("path").attr("d",
            function(d) {
                var x1 = d.source.x,
                    y1 = d.source.y,
                    x2 = d.target.x,
                    y2 = d.target.y,
                    dx = x2 - x1,
                    dy = y2 - y1,
                    dr = Math.sqrt(dx * dx + dy *
                        dy),

                    // Defaults for normal edge.
                    drx = dr,
                    dry = dr,
                    xRotation = 0, // degrees
                    largeArc = 0, // 1 or 0
                    sweep = 1; // 1 or 0

                // Self edge.
                if (x1 === x2 && y1 === y2) {
                    // Fiddle with this angle to get loop oriented.
                    xRotation = -45;

                    // Needs to be 1.
                    largeArc = 1;

                    // Change sweep to change orientation of loop. 
                    //sweep = 0;

                    // Make drx and dry different to get an ellipse
                    // instead of a circle.
                    drx = 60;
                    dry = 40;

                    // For whatever reason the arc collapses to a point if the beginning
                    // and ending points of the arc are the same, so kludge it.
                    x2 = x2 + 1;
                    y2 = y2 + 1;
                }

                return "M" + x1 + "," + y1 + "A" +
                    drx + "," + dry + " " +
                    xRotation + "," + largeArc +
                    "," + sweep + " " + x2 + "," +
                    y2;
            });



        linkGroups.selectAll(".eventText")
            .attr("transform", function(d) {
                if (d.source.x == d.target.x)
                    return "translate(" + (d.source.x + 25) + "," + (d.source.y - 45) + ")";
                else
                    return "translate(" + ((d.source.x + d.target.x) / 2) + "," + ((d.source.y + d.target.y) / 2) + ")";
            });


        groups.attr("transform", function(d) {
            return "translate(" + d.x + "," +
                d.y + ")";
        });
    });
});
