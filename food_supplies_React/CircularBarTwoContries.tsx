import type { PlanetaryComp } from "@/components/data-vis/planetary-comparison/planetaryComparison.tsx";
import React, { type FC, useEffect, useRef, useState } from "react";
import { scaleBand, scaleRadial } from "d3";
import { OneBar } from "./OneBar";
import { TooltipCountry } from "@/components/data-vis/planetary-comparison/Tooltip.tsx";
import type { TooltipDataCountry } from "@/components/data-vis/planetary-comparison/shared.tsx";
import { RadialScaleForBars } from "@/components/data-vis/planetary-comparison/RadialScaleForBars.tsx";

interface CircularBarTwoCountriesProps {
  data: PlanetaryComp[];
  lancet: PlanetaryComp[];
  energyFilter: string;
  isMobile: boolean;
}

export const isLancet = (country: string) => country === "Lancet";

export const CircularBarTwoCountries: FC<CircularBarTwoCountriesProps> = ({ data, energyFilter, isMobile, lancet }) => {
  const divElement = useRef<HTMLDivElement>(null);
  const [tooltipDataCountry, setTooltipDataCountry] = useState<TooltipDataCountry | null>(null);

  const width = divElement.current?.getBoundingClientRect().width || 10;
  const height = width;

  const groups = lancet
    .filter((d) => d.country)
    .sort((a, b) => b.grams - a.grams)
    .map((d, country) => d.food);

  const margin = { left: 0, right: 0, top: 0, bottom: 0 };

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
      ...lancet
        .filter((d) => d.country)
        .map((d: PlanetaryComp): number => (energyFilter === gram ? d.grams : d.calories))
    )
  );

  const innerRadius = isMobile ? 7 : 8;
  const outerRadius = Math.min(innerWidth, innerHeight) / 3;

  const yScale = scaleRadial()
    .domain([0, max || 10])
    .range([innerRadius, outerRadius]);

  return (
    <div ref={divElement} className={`relative overflow-visible w-full`}>
      <svg width={width} height={height}>
        <g transform={`translate(${margin.left}, ${margin.top})`}>
          {/*<rect width={width} height={height} fill={"red"} />*/}
          <g transform={`translate(${innerWidth / 2.15}, ${innerHeight / 2})`}>
            <RadialScaleForBars yScale={yScale} energyFilter={energyFilter} isMobile={isMobile} withText={false} />
            {data.map((one, i) => {
              return (
                <g className="overflow-visible " key={i}>
                  <OneBar
                    one={one}
                    lancet={lancet.find((d) => d.food === one.food)!}
                    xScale={xScale}
                    yScale={yScale}
                    innerRadius={innerRadius}
                    energyFilter={energyFilter}
                    setTooltipDataCountry={setTooltipDataCountry}
                  />
                </g>
              );
            })}
          </g>
        </g>
      </svg>
      <TooltipCountry
        width={width}
        height={height}
        tooltipDataCountry={tooltipDataCountry}
        isMobile={isMobile}
        energyFilter={energyFilter}
      />
    </div>
  );
};
