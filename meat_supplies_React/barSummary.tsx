import { scaleBand, scaleLinear } from "d3";
import { OneBar } from "@/components/data-vis/planetary-comparison/OneBar.tsx";
import React, { useRef } from "react";
import { colorCoding, colorCodingSummary, typeCoding } from "@/components/data-vis/planetary-comparison/shared.tsx";

export const summaryLancet = [
  { name: "plant", value: 1000 },
  { name: "animal", value: 300 },
  { name: "oil", value: 50 }
];

type summaryBarProps = {
  Lancet: { name: string; value: number }[];
  dataCountry: { name: string; value: number }[];
  energyFilter: string;
  maxDomainValue: number;
  isMobile: boolean;
};

export const BarSummary = ({ Lancet, dataCountry, energyFilter, maxDomainValue, isMobile }: summaryBarProps) => {
  const divElement = useRef<HTMLDivElement>(null);

  const width = divElement.current?.getBoundingClientRect().width || 10;
  const height = width * 0.8;
  const gram = "gram";
  const margin = { left: 32, right: 30, top: 30, bottom: energyFilter === gram ? 20 : 17 };
  const barWidth = width - margin.left - margin.right;
  const barHeight = height - margin.top - margin.bottom;
  const xScale = scaleBand().domain(["plant", "animal", "oil"]).range([0, barWidth]).padding(0.1);
  const yScale = scaleLinear().domain([0, maxDomainValue]).range([0, barHeight]);
  const arraySummaryGram = energyFilter === gram ? [100, 500, 900] : [300, 800, 1300];
  const lineAdd = arraySummaryGram.map((v, i) => {
    const y = yScale(v);
    return (
      <g key={i}>
        <line
          x1={0}
          x2={barWidth * 0.9}
          y1={y}
          y2={y}
          stroke="#575454"
          strokeWidth={0.5}
          strokeOpacity={0.5}
          pointerEvents="none"
          strokeDasharray="6 2"
        />
      </g>
    );
  });
  return (
    <div ref={divElement} className={""}>
      <svg width={width} height={height}>
        <g id={"countries"} transform={`translate(${margin.left}, ${margin.top})`}>
          {lineAdd}
          {dataCountry.map((country, i) => {
            const color = colorCoding(country.name);
            const colorSummary = colorCodingSummary(country.name);
            const gram = "gram";
            return (
              <g key={i}>
                <rect
                  x={xScale(country.name)}
                  y={yScale(0)}
                  width={xScale.bandwidth() * 0.6}
                  height={yScale(country.value)}
                  fill={colorSummary}
                />
                <text
                  className={"font-bodyBold"}
                  x={(xScale(country.name) ?? 0) + xScale.bandwidth() * 0.4}
                  y={yScale(country.value) + (isMobile ? 8 : 6)}
                  fontSize={isMobile ? 16 : 11}
                  fill={colorSummary}
                  textAnchor="middle"
                  alignmentBaseline="central"
                >
                  {`${country.value}${energyFilter === gram ? "g" : "kcal"}`}
                </text>
              </g>
            );
          })}
        </g>
        <g id={"lancet"} transform={`translate(${margin.left}, ${margin.top})`}>
          {Lancet.map((dpoint, i) => {
            const color = colorCoding(dpoint.name);
            const typeFull = typeCoding(dpoint.name);
            return (
              <g key={i}>
                <line
                  x1={xScale(dpoint.name) ?? 0}
                  y1={yScale(dpoint.value)}
                  x2={(xScale(dpoint.name) ?? 0) + xScale.bandwidth() * 0.6}
                  y2={yScale(dpoint.value)}
                  stroke={"rgba(0,0,0,0.8)"}
                  strokeWidth={1.5}
                  // strokeDasharray="6 2"
                />
                <rect
                  x={xScale(dpoint.name)}
                  y={yScale(-8)}
                  width={xScale.bandwidth() * 0.6}
                  height={yScale(dpoint.value)}
                  fill={"#faf7ed"}
                  fillOpacity={0.5}
                />
                {isMobile && (
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
                          <tspan
                            key={i}
                            x={(xScale(dpoint.name) ?? 0) + xScale.bandwidth() * 0.3}
                            dy={i === 0 ? 0 : 14}
                          >
                            {line}
                          </tspan>
                        ))
                      : typeFull}
                  </text>
                )}
              </g>
            );
          })}
        </g>
      </svg>
    </div>
  );
};
