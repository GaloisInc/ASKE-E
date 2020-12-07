import React, { useEffect, useRef, useState, KeyboardEvent } from 'react';
import * as d3 from 'd3';

import '../styles/HelloD3.scss';

const DATA = [5, 3, 6, 1, 2];

export const HelloD3: React.FC<{ id: string }> = ({ id }) => {
    const svgRef = useRef(null);
    const [data, setData] = useState([...DATA]);

    const update = () => {
        const newData = data.map((value) => value + 5);
        setData(newData);
    };

    const removeLast = () => {
        const newData = [...data];
        newData.pop();
        setData(newData.length > 0 ? newData : [...DATA]);
    };

    useEffect(() => {
        const handler = (e: KeyboardEvent): any => {
            switch (e.key) {
                case 'u':
                    update();
                    break;
                case 'r':
                    removeLast();
                    break;
            }

            e.preventDefault();
        };

        document.addEventListener('keydown', handler as any, false);

        // you return a clean-up function that will be called when the component is unmounted
        return () => document.removeEventListener('keydown', handler as any);

        // we set the "watch list" to an empty array to cause it to be run exactly once
        // eslint-disable-next-line react-hooks/exhaustive-deps
    });

    useEffect(() => {
        // this is a "react hook" that is run whever component is mounted into the DOM
        const svg = d3.select(svgRef.current); // select svg ref

        svg.selectAll('rect')
            .data(data)
            .join(
                (enter) => enter.append('rect').attr('class', 'new-element'),
                (update) => update.attr('class', 'updated-element'),
                (exit) =>
                    exit
                        .transition()
                        .attr('width', 0)
                        .attr('height', 0)
                        .remove()
            )
            .transition()
            .duration(1000)
            .attr('width', (value) => value * 10)
            .attr('height', 50)
            .attr('x', (value) => value + 10)
            .attr('y', (value) => (value * value) / 2)
            .attr('stroke', 'red')
            .attr('stroke-width', '3')
            .attr('fill', 'transparent');
    }, [data, svgRef]);

    return (
        <div className='HelloD3'>
            <h4>type "u" to update squares and "r" to undo</h4>
            <svg ref={svgRef}></svg>
        </div>
    );
};
