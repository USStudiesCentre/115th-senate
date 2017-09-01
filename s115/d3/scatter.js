var margin = {top: 40, right: 12, bottom: 40, left: 24};
var width = 1000 - margin.left - margin.right;
var height = 700 - margin.top - margin.bottom;

var dataset;
var demLoess;
var repLoess;

var svg = d3.select("body").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var tip = d3.tip()
    .attr('class', 'd3-tip')
    .offset([-10, 0])
    .html(function(d) {
	return "<span>" + d.legisNames + "</span>";
    }
	 );

svg.call(tip);

queue()
    .defer(function(url,callback){
	d3.csv(url,
	       function(error,data){
		   demLoess = data;
		   callback(error,data);
	       });
	}, "../data/demLoess.csv")
    .defer(function(url,callback){
	d3.csv(url,
	       function(error,data){
		   data.x = +data.x;
		   data.yhat = +data.yhat;
		   repLoess = data;
		   callback(error,data);
	       });
    },
	   "../data/repLoess.csv")
    .defer(function(url,callback){
	d3.csv(url,
	       function(error,data){
		   dataset = data;
		   callback(error,data);
	       });
	},
	   "../data/plotData.csv")
    .await(ready);


function ready(error) {
    //console.log(dataset);
    //console.log(demLoess);
    //console.log(repLoess);
    //console.log(ft);

    colorValue = function(dataset) {
	return dataset.party; 
    },
    color = d3.scale.ordinal()
	.domain(["R","D","I"])
	.range(["red","blue","green"]);
    
    var xmin = d3.min(dataset, function(d) { return +d.x; });
    var xmax = d3.max(dataset, function(d) { return +d.x; });
    var xrange = xmax-xmin;
		      
    var ymin = d3.min(dataset, function(d) { return +d.y; });
    var ymax = d3.max(dataset, function(d) { return +d.y; });
    var yrange = ymax - ymin;
    var yup = ymax + (.015 * yrange);
    var ylo = ymin - (.015 * yrange);

    var xScale = d3.scale.linear()
	.range([0, width])
	.domain([xmin - .015*xrange, xmax + .015*xrange]);
		      
    var yScale = d3.scale.linear()
	.range([height, 0])
	.domain([ymin - .015*yrange, ymax + .015*yrange]);
    
    var yTickValues = d3.range( -2.0, 1.6, 0.5 );
    var xTickValues = d3.range(20,91,10);
 
    // axes grids
    var formatAsPercentage = d3.format("");		      
    var xAxis = d3.svg.axis()
	.scale(xScale)
	.tickSize(0, 0, 0)
	.tickValues(xTickValues)
	.tickFormat(formatAsPercentage)
	.orient("bottom");
		      
    var xAxis2 = d3.svg.axis()
	.scale(xScale)
	.tickSize(0, 0, 0)
	.tickValues(xTickValues)
	.tickFormat(formatAsPercentage)
	.orient("top");
		      
    var yAxis = d3.svg.axis()
	.scale(yScale)
	.tickSize(0, 0, 0)
	.tickValues(yTickValues)
	.tickFormat(d3.format("> .2f"))
	.orient("left");

    var y2Axis = d3.svg.axis()
	.scale(yScale)
	.tickSize(0, 0, 0)
	.tickValues(yTickValues)
	.tickFormat(d3.format("> .2f"))
	.orient("right");
    
    svg.append("g")
	.attr("class", "x.axis")
	.attr("transform", "translate(10," + (height - 6) + ")")
	.style("font-size", "14px")
	.call(xAxis);
		      
    svg.append("g")
	.attr("class", "x.axis")
	.attr("transform", "translate(10,6)")
	.style("font-size", "14px")
	.call(xAxis2)
    
    function make_x_grid(){
	return d3.svg.axis()
	    .scale(xScale)
	    .orient("bottom")
    }
    
    function make_y_grid(){
	return d3.svg.axis()
	    .scale(yScale)
	    .orient("left")
    }

    svg.append("g")         
	.attr("class", "grid")
	.style("opacity",0.15)
	.call(make_x_grid()
	      .tickSize(height, 0, 0)
	      .tickFormat("")
	     );
    
    svg.append("g")         
	.attr("class", "grid")
	.style("opacity",0.15)
	.call(make_y_grid()
	      .tickSize(-width, 0, 0)
	      .tickFormat("")
	     );
		      

    // loess lines
    var loessLine = d3.svg.line()
	.x(function(d) { return xScale(d.x); })
	.y(function(d) { return yScale(d.yhat); });
    
    svg.append("path")
	.datum(demLoess)
	.attr("class", "line")
	.attr("d",loessLine);
			  
    svg.append("path")
	.datum(repLoess)
	.attr("class", "line")
	.attr("d",loessLine);
 
    
    // plot data
    svg.selectAll("rect")
	.data(dataset)
	.enter()
	.append("rect")
	.attr("class","rect")
	.attr("x",function(d){
	    return xScale(d.x) + 4.5;
	})
	.attr("y",function(d){
	    return yScale(d.y) + 4.5;
	})
	.attr("width",9)
	.attr("height",9)
	.style("opacity",.25)
	.style("fill",function(d) { return color(colorValue(d)); } )
	.on('mouseover', 
	    function(d) {
		tip.show(d);
		d3.select(this).style("opacity",.85);
	    })
	.on('mouseout', 
	    function(d) { 
		d3.select(this).style("opacity",.25);
		tip.hide(d);
	    }
	   )
    
    // labelling
    svg.append("text")
	.attr("x",width/2)
	.attr("y",height + 36)
	.text("Trump share of presidential vote, 2016 election (%)")
	.attr("text-anchor", "middle")
	.style("font-size", "16px");
    
    // titling
    svg.append("text")
	.attr("class", "title")
	.attr("x", -12)
	.attr("y", -16)
	.text("Ideal points, 115th U.S. Senate, by Trump 2016 vote in state");
    
    svg.append("text")
	.attr("class", "credit")
	.attr("x",width)
	.attr("y",24)
	.attr("text-anchor","end")
	.text("Computed by Simon Jackman, " + out_formatTime(ft[0].mtime) + ".");

    svg.append("text")
	.attr("class", "credit")
	.attr("x",width)
	.attr("y",40)
	.attr("text-anchor","end")
	.text("Analysis of " + nvotes[0].nvotes + " non-unanimous roll calls.");
    
    svg.append("text")
	.attr("class", "credit")
	.attr("x",width)
	.attr("y",56)
	.attr("text-anchor","end")
	.text("Solid lines are local linear regressions, fit to each party.");
		      
    svg.append("g")
	.append("svg:a")
	.attr({
	    "xlink:href": "http://dx.doi.org/10.1017/S0003055404001194",
	    "target": "_blank"})
	.append("svg:text")
	.attr("x",width)
	.attr("y",72)
	.style("fill","#aaa")
	.on('mouseover', function(d){
                d3.select(this).style("text-decoration","underline");
                d3.select(this).style("fill","blue");
            })
        .on('mouseout', function(d){
                d3.select(this).style("text-decoration","none");
                d3.select(this).style("fill","#aaa");
            })
	.attr("text-anchor","end")
	.text("Methodological details: Clinton, Jackman & Rivers, APSR 2004.");
}
  

