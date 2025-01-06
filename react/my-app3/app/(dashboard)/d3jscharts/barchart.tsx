

import * as d3 from "d3";
import { useEffect,useState, useRef } from "react";
interface DataPoint {
    Country: string;
    Value: number;
}
const url = "https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/7_OneCatOneNum_header.csv"
const width = 500;
const height = 400;

async function loadData(filePath: string): Promise<DataPoint[]> {
    return d3.csv<DataPoint>(filePath, (d) => ({
        Country: d.Country,
        Value: +d.Value, // Convert string to number
    }));
  }
  
  const BarChart: React.FC = () => {
    const [data, setData] = useState<DataPoint[]>([]);
    const svgRef = useRef(null);
  
    useEffect(() => {
      const fetchData = async () => {
        try {
          
          const data = await loadData(url)

          setData(data);
        } catch (error) {
          console.error('Error loading data:', error);
        }
      };
  
      fetchData();
    }, []);
  
    useEffect(() => {
      if (!data || !svgRef.current) return;
  
      const svg = d3.select(svgRef.current);
      
      const margin = { top: 20, right: 20, bottom: 100, left: 40 };
  
      const x = d3
        .scaleBand()
        .domain(data.map((d) => d.Country))
        .range([margin.left, width - margin.right])
        .padding(0.1);
  
      const y = d3
        .scaleLinear()
        .domain([0, d3.max(data, (d) => d.Value) || 0])
        .range([height - margin.bottom, margin.top]);
  
      svg
        .selectAll('.bar')
        .data(data)
        .join('rect')
        .attr('class', 'bar')
        .attr('x', (d) => x(d.Country))
        .attr('y', (d) => y(d.Value))
        .attr('width', x.bandwidth())
        .attr('height', (d) => height - margin.bottom - y(d.Value))
        .attr('fill', 'steelblue');
  
      svg
        .append('g')
        .attr('transform', `translate(0,${height - margin.bottom})`)
        .call(d3.axisBottom(x))
        .selectAll("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("x", -5)
        .style("text-anchor", "end"); 
  
      svg
        .append('g')
        .attr('transform', `translate(${margin.left},0)`)
        .call(d3.axisLeft(y));
    }, [data]);
  
    return (
      <svg ref={svgRef} width={width} height={height}></svg>
    );
  };
  
  export default BarChart;