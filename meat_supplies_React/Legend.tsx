import { PlanetaryChart } from "@/components/data-vis/planetary-comparison/PlanetaryChart.tsx";
import type { TooltipData } from "@/components/data-vis/planetary-comparison/shared.tsx";
import { Tooltip } from "@/components/data-vis/planetary-comparison/Tooltip.tsx";
import { BarSummaryPlanetary } from "@/components/data-vis/planetary-comparison/barSummaryPlanetary.tsx";
import React, { useRef, useState } from "react";
import type { PlanetaryComp } from "@/components/data-vis/planetary-comparison/planetaryComparison.tsx";

interface PlanetaryLegendProps {
  legendData: any;
  groupedLegendData: any;
  energyFilter: string;
  isMobile: boolean;
}

export const PlanetaryLegend = ({ legendData, groupedLegendData, energyFilter, isMobile }: PlanetaryLegendProps) => {
  const legendDiv = useRef<HTMLDivElement>(null);
  const [tooltipData, setTooltipData] = useState<TooltipData | null>(null);

  const width = legendDiv.current?.clientWidth || 0;
  const height = width * 1;
  // className={"underline decoration-1 underline-offset-2 decoration-dashed decoration-gray-500"}
  // @ts-ignore
  // @ts-ignore
  return (
    <div ref={legendDiv} className={"bg-[#faf7ed] shadow-sm shadow-gray-300"}>
      {/*<div className="absolute top-4 left-3 w-3 h-3 rounded-full bg-site-background mb-0 border border-1 border-gray-400"></div>*/}
      {legendData.map(({ country, data, summary }) => (
        <div key={country} className="flex flex-col items-center">
          <h3 className="text-base-md font-bodyVeryBold text-blackSt mt-[16px]">Planetary diet targets</h3>
          <p className="text-base font-body text-blackSt leading-none text-center mb-2 mx-2">
            The planetary health diet promotes consumption of vegetables, fruits, cereals, and legumes. It also allows
            modest amounts of animal foods, such as dairy and meat, along with added oils and fats. The overall target
            is about 2,500 kcal per person per day.
          </p>
          <div className="text-base font-bodyVeryBold text-blackSt mt-4 w-[80%] leading-none text-center ">
            Recommended daily intake by food group, per person
          </div>
          <div>
            <PlanetaryChart
              data={data}
              isMobile={isMobile}
              energyFilter={energyFilter}
              width={width}
              height={height}
              setTooltipData={(tooltipData: TooltipData | null) => {
                setTooltipData(tooltipData);
              }}
            />
            <Tooltip
              width={width}
              height={legendDiv.current?.clientHeight || width}
              tooltipData={tooltipData}
              isMobile={isMobile}
              energyFilter={energyFilter}
            />
          </div>
          <div className="text-base font-bodyVeryBold text-blackSt w-[80%] leading-none text-center my-6">
            Summary by source
          </div>
          <div className={"mr-[-1rem] overflow-visible"}>
            <BarSummaryPlanetary
              width={width * 0.8}
              height={height * 0.5}
              Lancet={groupedLegendData}
              energyFilter={energyFilter}
              isMobile={isMobile}
            />
          </div>
        </div>
      ))}
    </div>
  );
};
