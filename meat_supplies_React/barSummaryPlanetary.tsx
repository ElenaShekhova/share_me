import { scaleBand, scaleLinear } from "d3";
import { OneBar } from "@/components/data-vis/planetary-comparison/OneBar.tsx";
import React from "react";
import { colorCoding, colorCodingSummary, typeCoding } from "@/components/data-vis/planetary-comparison/shared.tsx";

export const summaryLancet = [
  { name: "plant", value: 1000 },
  { name: "animal", value: 300 },
  { name: "oil", value: 50 }
];

type summaryBarProps = {
  width: number;
  height: number;
  Lancet: { name: string; value: number }[];
  energyFilter: string;
  isMobile: boolean;
};

export const BarSummaryPlanetary = ({ width, height, Lancet, energyFilter, isMobile }: summaryBarProps) => {
  const gram = "gram";
  const margin = { left: 32, right: 30, top: 30, bottom: energyFilter === gram ? 10 : 60 };
  const barWidth = width - margin.left - margin.right;
  const barHeight = height - margin.top - margin.bottom;
  const xScale = scaleBand().domain(["plant", "animal", "oil"]).range([0, barWidth]).padding(0.1);
  const yScale = scaleLinear().domain([0, 1200]).range([0, barHeight]);

  const arraySummaryGram = energyFilter === gram ? [100, 500, 900] : [300, 800, 1300];
  const lineAdd = arraySummaryGram.map((v, i) => {
    const y = yScale(v);
    return (
      <g key={i}>
        <line
          x1={0}
          x2={barWidth}
          y1={y}
          y2={y}
          stroke="#575454"
          strokeWidth={0.5}
          strokeOpacity={0.5}
          pointerEvents="none"
          strokeDasharray="6 2"
        />
        <text
          x={barWidth - 10}
          y={y + 6}
          className={"font-body"}
          fill={"rgba(0,0,0,0.8)"}
          fontSize={isMobile ? 8 : 10}
          alignmentBaseline="middle"
        >
          {`${v}${energyFilter === gram ? "g" : "kcal"}`}
        </text>
      </g>
    );
  });
  return (
    <div className={"overflow-visible"}>
      <svg width={width} height={height}>
        <g id={"lancet"} transform={`translate(${margin.left}, ${margin.top})`}>
          {lineAdd}
          {Lancet.map((dpoint, i) => {
            const colorSummary = colorCodingSummary(dpoint.name);
            const color = colorCoding(dpoint.name);
            const typeFull = typeCoding(dpoint.name);
            const gram = "gram";
            return (
              <g key={i}>
                <rect
                  x={xScale(dpoint.name)}
                  y={yScale(0)}
                  width={xScale.bandwidth() * 0.6}
                  height={yScale(dpoint.value)}
                  fill={color}
                  fillOpacity={1}
                />
                <text
                  className="font-bodyBold"
                  x={(xScale(dpoint.name) ?? 0) + xScale.bandwidth() * 0.3}
                  y={yScale(0) - 20}
                  fontSize={isMobile ? 12 : 14}
                  textAnchor="middle"
                  alignmentBaseline="hanging"
                  fill={"rgba(0,0,0,0.8)"}
                >
                  {Array.isArray(typeFull)
                    ? typeFull.map((line, i) => (
                        <tspan key={i} x={(xScale(dpoint.name) ?? 0) + xScale.bandwidth() * 0.3} dy={i === 0 ? 0 : 14}>
                          {line}
                        </tspan>
                      ))
                    : typeFull}
                </text>
                <line
                  x1={xScale(dpoint.name) ?? 0}
                  y1={yScale(dpoint.value)}
                  x2={(xScale(dpoint.name) ?? 0) + xScale.bandwidth() * 0.6}
                  y2={yScale(dpoint.value)}
                  stroke={"rgba(0,0,0,0.8)"}
                  strokeWidth={1.5}
                  // strokeDasharray="6 2"
                />
                <text
                  className={"font-bodyBold"}
                  x={(xScale(dpoint.name) ?? 0) + xScale.bandwidth() * 0.8}
                  y={yScale(dpoint.value) + 8}
                  fontSize={12}
                  textAnchor="end"
                  alignmentBaseline="central"
                  fill={"rgba(0,0,0,0.8)"}
                >
                  {`${dpoint.value}${energyFilter === gram ? "g" : "kcal"}`}
                </text>
              </g>
            );
          })}
        </g>
      </svg>
    </div>
  );
};
