import type { PlanetaryComp } from "@/components/data-vis/planetary-comparison/planetaryComparison.tsx";
import React, { type FC } from "react";
import { scaleBand, scaleRadial } from "d3";
import { OneBarPlanetary } from "@/components/data-vis/planetary-comparison/OneBarPlanetary.tsx";
import type { TooltipData } from "./shared";
import { RadialScaleForBars } from "@/components/data-vis/planetary-comparison/RadialScaleForBars.tsx";

interface CircularBarTwoCountriesProps {
  data: PlanetaryComp[];
  energyFilter: string;
  width: number;
  height: number;
  isMobile: boolean;
  setTooltipData: (tooltipData: TooltipData | null) => void;
}

export const isLancet = (country: string) => country === "Lancet";

export const PlanetaryChart: FC<CircularBarTwoCountriesProps> = ({
  data,
  energyFilter,
  isMobile,
  width,
  height,
  setTooltipData
}) => {
  const groups = data.sort((a, b) => b.grams - a.grams).map((d, country) => d.food);

  const margin = { left: 5, right: 5, top: 20, bottom: 20 };

  const innerWidth = width - margin.left - margin.right;
  const innerHeight = height - margin.top - margin.bottom;
  const gram = "gram";
  const calorie = "calorie";
  const BAR_PADDING = 0.2;
  const xScale = scaleBand()
    .domain(groups)
    .range([0, 2 * Math.PI])
    .padding(BAR_PADDING);

  const max = Math.floor(
    Math.max(
      ...data
        .filter((d) => isLancet(d.country))
        .map((d: PlanetaryComp): number => (energyFilter === gram ? d.grams : d.calories))
    )
  );

  const innerRadius = isMobile ? 10 : 15;
  const outerRadius = Math.min(innerWidth, innerHeight) / 3;

  const yScale = scaleRadial()
    .domain([0, max || 10])
    .range([innerRadius, outerRadius]);

  return (
    <div className={`relative overflow-visible w-[${width}px]`}>
      <svg width={width} height={height}>
        <g transform={`translate(${margin.left}, ${margin.top})`}>
          <g transform={`translate(${innerWidth / 2}, ${innerHeight / 2})`}>
            <RadialScaleForBars yScale={yScale} energyFilter={energyFilter} isMobile={isMobile} withText={true} />
            {data.map((one, i) => {
              return (
                <g key={i}>
                  <OneBarPlanetary
                    one={one}
                    xScale={xScale}
                    yScale={yScale}
                    innerRadius={innerRadius}
                    energyFilter={energyFilter}
                    isMobile={isMobile}
                    setTooltipData={setTooltipData}
                    width={width}
                    height={height}
                  />
                </g>
              );
            })}
          </g>
        </g>
      </svg>
    </div>
  );
};
